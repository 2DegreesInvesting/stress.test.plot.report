#' creates a dataframe where each row is a set of parameters to be used as input.
#' Baseline and shock scenario are always included in the columns, the other columns are
#' the parameters names defined in the param_grid.
#' The values inside the param_grid are repeated for each pair of baseline/shock scenario.
#' The values inside the param_grid are combined by duets, triplets, etc..
#'  depending on the max_param_combinations value. The values of the parameters are
#'  tweaked one at a time if max_param_combinations=1 .
#'
#' @return a dataframe where each row is a set of parameters for 1 TRISK run
generate_runs_parameters <- function(scenario_pairs, params_grid) {
  runs_parameters <- scenario_pairs |> dplyr::cross_join(expand.grid(params_grid))
  runs_parameters
}

#' Get all runs already completed in the current experiment defined by its name.
#' @return a dataframe where each row is a set of parameters that have been completed for 1 TRISK run
fetch_completed_runs_parameters <- function(runs_parameters, tracking_uri, experiment_name) {
  mlflow::mlflow_set_tracking_uri(tracking_uri)
  experiment_id <- mlflow::mlflow_get_experiment(name = experiment_name)$experiment_id[1]
  # iterate the runs search by filtering over each baseline_scenario
  # because mlflow returns at most 1000 runs from a search. It will work
  # as long as there are less than 1000 runs per baseline scenario.
  completed_runs_params <- NULL
  for (baseline_scenario in unique(runs_parameters$baseline_scenario)) {
    finished_baseline_runs <- mlflow::mlflow_search_runs(
      filter = paste("params.baseline_scenario = '", baseline_scenario, "'",
        " and ",
        "attributes.status = 'FINISHED'",
        sep = ""
      ),
      experiment_ids = as.character(experiment_id)
    )
    if (nrow(finished_baseline_runs) == 1000) {
      stop("mlflow_search_runs has reached the limit of runs that can be returned with this filter.
           Must refine the filter conditions in generate_runs_parameters().")
    }
    if (nrow(finished_baseline_runs) > 0) {
      # lengthy block of code to convert the output from MLFlow
      # to the same dataframe format as the `runs_parameters` dataframe
      params_df_list <- lapply(
        seq_len(nrow(finished_baseline_runs)),
        function(x) {
          (dplyr::bind_rows(
            dplyr::inner_join(finished_baseline_runs$params[[x]],
              finished_baseline_runs$tags[[x]],
              by = "key", suffix = c(".params", ".tags")
            ),
            finished_baseline_runs$params[[x]] |>
              dplyr::filter(key %in% c("baseline_scenario", "shock_scenario")) |>
              dplyr::mutate(value.params = value, value.tags = "TRUE") |>
              dplyr::select(key, value.params, value.tags)
          ))
        }
      )
      params_df_list <- lapply(
        seq_len(length(params_df_list)),
        function(x) {
          (params_df_list[[x]] |>
            dplyr::mutate(value.params = ifelse(value.tags, value.params, NA)) |>
            dplyr::select(key, value.params))
        }
      )
      params_df_list <- lapply(
        seq_len(length(params_df_list)),
        function(x) {
          (tidyr::pivot_wider(params_df_list[[x]],
            names_from = "key",
            values_from = "value.params"
          ))
        }
      )
      completed_runs_params_baseline_scenario <- dplyr::bind_rows(params_df_list)

      # aggregate the baseline_scenario runs together
      completed_runs_params <- dplyr::bind_rows(
        completed_runs_params,
        completed_runs_params_baseline_scenario
      )
    }
  }
  completed_runs_params <- as.data.frame(completed_runs_params)
  completed_runs_params
}

#' removes rows from runs_parameters where an equivalent run has been completed
#' from completed_runs_params.
filter_out_completed_runs <- function(runs_parameters, completed_runs_params) {
  uncompleted_runs <- dplyr::anti_join(
    runs_parameters |> dplyr::mutate(dplyr::across(dplyr::everything(), purrr::partial(utils::type.convert, as.is = TRUE))),
    completed_runs_params |> dplyr::mutate(dplyr::across(dplyr::everything(), purrr::partial(utils::type.convert, as.is = TRUE))),
    by = names(runs_parameters)
  )

  uncompleted_runs
}

#' Generate a table of parameters for all runs, where each row defines the parameters
#' of 1 run. Filter this table of parameters if there are already completed runs
#' with a matching set in the mlflow experiment.
generate_and_filter_run_parameters <-
  function(tracking_uri,
           experiment_name,
           scenario_pairs,
           params_grid) {
    runs_parameters <- generate_runs_parameters(scenario_pairs, params_grid)
    print("Gathering previous runs parameters...")
    completed_runs_params <- fetch_completed_runs_parameters(runs_parameters, tracking_uri, experiment_name)
    if (nrow(completed_runs_params) > 0) {
      filtered_run_parameters <- filter_out_completed_runs(runs_parameters, completed_runs_params)
      print(paste(
        "Removed", nrow(runs_parameters) - nrow(filtered_run_parameters),
        "runs that have already been executed"
      ))
    } else {
      filtered_run_parameters <- runs_parameters
    }
    filtered_run_parameters
  }

#' Get or create MLFlow experiment from its name.
#' Gets mlflow experiment, or if it doesn't exist creates it, then set it.
set_mlflow_experiment <- function(experiment_name) {
  tryCatch(
    {
      mlflow::mlflow_get_experiment(name = experiment_name)
    },
    error = function(cond) {
      mlflow::mlflow_create_experiment(experiment_name)
    },
    finally = {
      mlflow::mlflow_set_experiment(experiment_name)
    }
  )
}

#' Creates a TRUE/FALSE flag for each parameter indicating which parameter
#' is being tweaked in the current run of trisk.
create_tags_list <- function(param_names, params_grid, additional_tags) {
  param_names <- param_names[param_names %in% names(params_grid)] # remove baseline and shock param
  param_tweaked <- rep(FALSE, length(params_grid))
  names(param_tweaked) <- names(params_grid)
  param_tweaked[param_names] <- TRUE
  tags <- c(param_tweaked, additional_tags)

  tags
}

log_metrics_df <- function(metrics_df) {
  for (i in seq_len(nrow(metrics_df))) {
    metric_name <- metrics_df[[i, "metric_name"]]
    metric_name <- stringr::str_replace_all(metric_name, "[^A-Za-z0-9]", "_")
    metric_value <- metrics_df[[i, "metric_value"]]
    mlflow::mlflow_log_metric(metric_name, metric_value)
  }
}


write_and_zip_csv_artifacts <-
  function(st_results_wrangled_and_checked,
           mlflow_run_output_dir,
           save_artifacts) {
    written_csv_paths <- NULL
    for (artifact_name in save_artifacts) {
      filepath <- file.path(
        mlflow_run_output_dir,
        paste(artifact_name, ".csv", sep = "")
      )
      zipfilepath <- file.path(
        mlflow_run_output_dir,
        paste(artifact_name, ".zip", sep = "")
      )
      readr::write_csv(st_results_wrangled_and_checked[[artifact_name]], filepath)
      zip::zip(zipfilepath, basename(filepath), root = mlflow_run_output_dir)

      unlink(filepath)
    }
  }

#' Aggregate numerical trajectories columns
#'
#' @param multi_trajectories dataframe of trajectories from 1 or multiple trisk truns
#' @param group_cols group_cols
#' @param param_cols param_cols
#'
#' @return
#' @export
#'
aggregate_trajectories_facts <-
  function(multi_trajectories, group_cols, param_cols = c(
             "run_id", "scenario_geography", "baseline_scenario",
             "shock_scenario", "year"
           )) {
    multi_trajectories <- multi_trajectories |>
      dplyr::group_by_at(unique(c(group_cols, param_cols))) |>
      dplyr::summarise(
        production_baseline_scenario = sum(production_baseline_scenario, na.rm = TRUE),
        production_target_scenario = sum(production_target_scenario, na.rm = TRUE),
        production_shock_scenario = sum(production_shock_scenario, na.rm = TRUE),
        .groups = "drop"
      )
    return(multi_trajectories)
  }

#' Convert trajectories from absolute values to percentage of the max
#'
#' @param multi_trajectories dataframe of trajectories from 1 or multiple trisk truns
#' @param group_cols group_cols
#'
#' @return
#' @export
#'
convert_trajectories_as_percentages <-
  function(multi_trajectories, group_cols) {
    multi_trajectories <- multi_trajectories |>
      dplyr::group_by_at(group_cols) |>
      dplyr::mutate(
        production_baseline_scenario = production_baseline_scenario / max(production_baseline_scenario),
        production_target_scenario = production_target_scenario / max(production_target_scenario),
        production_shock_scenario = production_shock_scenario / max(production_shock_scenario)
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(year < max(year))
    return(multi_trajectories)
  }

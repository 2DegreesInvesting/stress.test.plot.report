load_multiple_trajectories <- function(crispy_outputs_dir) {
  # Required Libraries

  # Get file paths
  files_path <- list.files(
    path = crispy_outputs_dir,
    pattern = "^company_trajectories_(.*).csv",
    recursive = TRUE,
    full.names = TRUE
  )

  stopifnot(length(files_path) > 0)

  # Load all files into a list and add a run_id column for each dataframe
  data_list <- purrr::map(files_path, function(fp) {
    df <- readr::read_csv(fp,
      col_types = readr::cols_only(
      run_id = "c",
      company_id = "c",
      company_name = "c",
      ald_sector = "c",
      ald_business_unit = "c",
      year = "d",
      production_baseline_scenario = "c",
      production_target_scenario = "c",
      production_shock_scenario = "c"
      )
    )
  })

  multi_trajectories_data <- dplyr::bind_rows(data_list)

  return(multi_trajectories_data)
}


#' Aggregate numerical trajectories columns
#'
#' @param multi_trajectories dataframe of trajectories from 1 or multiple trisk truns
#' @param group_cols group_cols
#'
#' @return
#' @export
#'
aggregate_trajectories_facts <-
  function(multi_trajectories, group_cols) {
    multi_trajectories <- multi_trajectories |>
      dplyr::group_by_at(group_cols) |>
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


#' Title
#'
#' @param crispy_outputs_dir
#'
#' @return
#' @export
#'
#' @examples
load_multiple_crispy <- function(crispy_outputs_dir, max_granularity) {
  # Required Libraries

  # Get file paths
  files_path <- list.files(
    path = crispy_outputs_dir,
    pattern = "^crispy_output_.*.parquet",
    recursive = TRUE,
    full.names = TRUE
  )

  # Load all files into a list and add a run_id column for each dataframe
  data_list <- purrr::map(files_path, function(file_path) {
    df <- arrow::read_parquet(file_path) |>
      dplyr::mutate(
        run_id=file_path
          )|>
      dplyr::mutate(
        run_id=stringr::str_remove(basename(.data$run_id), ".parquet")
      ) |>
      dplyr::mutate(
        run_id=stringr::str_remove(.data$run_id, "crispy_output_standard_")
      )
  })


  multi_crispy <- dplyr::bind_rows(data_list)

  return(multi_crispy)
}




#' Title
#'
#' # TODO FIND CLOSEST COMPANY IF group_cols=NULL
#'
#' @param multi_crispy
#' @param group_cols
#'
#' @return
#' @export
#'
#' @examples
aggregate_crispy_facts <- function(multi_crispy, group_cols) {

  multi_crispy <- multi_crispy|>
    dplyr::group_by_at(group_cols) |>
    dplyr::summarise(
      net_present_value_baseline=stats::median(net_present_value_baseline, na.rm=T),
      net_present_value_shock=stats::median(net_present_value_shock, na.rm=T),
      pd_baseline=stats::median(pd_baseline, na.rm=T),
      pd_shock=stats::median(pd_shock, na.rm=T),
      .groups="drop"
    )
  return(multi_crispy)
}

get_trisk_params <- function(multi_crispy){
  trisk_runs_params <- multi_crispy |>
    dplyr::distinct(.data$run_id, .data$roll_up_type, .data$baseline_scenario, .data$shock_scenario,
                    .data$lgd, .data$risk_free_rate, .data$discount_rate, .data$dividend_rate, .data$growth_rate,
                    .data$shock_year)

    stopifnot(nrow(
      trisk_runs_params |>
        dplyr::select(-c(.data$run_id)) |>
        dplyr::distinct_all()) == length(unique(trisk_runs_params$run_id)))

    return(trisk_runs_params)
}

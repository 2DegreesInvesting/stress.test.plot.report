
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
    pattern = "^crispy_output_.*.csv",
    recursive = TRUE,
    full.names = TRUE
  )

  # Load all files into a list and add a run_id column for each dataframe
  data_list <- purrr::map(files_path, function(fp) {
    df <- readr::read_csv(fp) |>
      dplyr::mutate(
        run_id = sub("^crispy_output_standard_(.*)\\.csv", "\\1", basename(fp))
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
aggregate_crispy_facts <- function(multi_crispy, group_cols, param_cols=c("run_id","roll_up_type", "baseline_scenario", "shock_scenario",
                                                                          "risk_free_rate", "discount_rate", "dividend_rate", "growth_rate",
                                                                          "shock_year")) {

  multi_crispy <- multi_crispy|>
    dplyr::group_by_at(c(group_cols, param_cols)) |>
    dplyr::summarise(
      net_present_value_baseline=sum(net_present_value_baseline, na.rm=T),
      net_present_value_shock=sum(net_present_value_shock, na.rm=T),
      pd_baseline=stats::median(pd_baseline, na.rm=T),
      pd_shock=stats::median(pd_shock, na.rm=T),
      .groups="drop"
    )
  return(multi_crispy)
}


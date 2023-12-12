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
    pattern = "^crispy_output_(.*).csv",
    recursive = TRUE,
    full.names = TRUE
  )

  # Load all files into a list and add a run_id column for each dataframe
  data_list <- purrr::map(files_path, function(fp) {
    df <- readr::read_csv(fp,
      col_types = readr::cols_only(
        run_id = "c",
        term = "d",
        scenario_geography = "c",
        company_id = "c",
        company_name = "c",
        ald_sector = "c",
        ald_business_unit = "c",
        roll_up_type = "c",
        baseline_scenario = "c",
        shock_scenario = "c",
        risk_free_rate = "d",
        discount_rate = "d",
        dividend_rate = "d",
        growth_rate = "d",
        shock_year = "d",
        net_present_value_baseline = "d",
        net_present_value_shock = "d",
        pd_baseline = "d",
        pd_shock = "d"
      )
    )
  })

  multi_crispy_data <- dplyr::bind_rows(data_list)

  return(multi_crispy_data)
}




#' Title
#'
#' # TODO FIND CLOSEST COMPANY IF group_cols=NULL
#'
#' @param multi_crispy
#' @param group_cols
#' @param param_cols TODO the parameters should be stored in a dataframe other than crispy and use the run_id as key to join
#'
#' @return
#'
#' @examples
aggregate_crispy_facts <- function(multi_crispy, group_cols, param_cols = c(
                                     "run_id", "roll_up_type", "baseline_scenario", "shock_scenario",
                                     "risk_free_rate", "discount_rate", "dividend_rate", "growth_rate",
                                     "shock_year"
                                   )) {
  multi_crispy <- multi_crispy |>
    dplyr::group_by_at(c(group_cols, param_cols)) |>
    dplyr::summarise(
      net_present_value_baseline = sum(net_present_value_baseline, na.rm = T),
      net_present_value_shock = sum(net_present_value_shock, na.rm = T),
      pd_baseline = stats::median(pd_baseline, na.rm = T),
      pd_shock = stats::median(pd_shock, na.rm = T),
      .groups = "drop"
    )
  return(multi_crispy)
}

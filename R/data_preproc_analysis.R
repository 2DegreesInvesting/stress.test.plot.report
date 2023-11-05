#' Title
#'
#' ## TODO HOW TO HANDLE ENTRIES IN PORTFOLIO, THAT ARE MISSING IN CRISPY ?
#'
#' @param portfolio_data
#' @param multi_crispy
#'
#' @return
#' @export
#'
#' @examples
create_analysis_data <- function(portfolio_data, multi_crispy, portfolio_crispy_merge_cols) {
  analysis_data <-
    dplyr::inner_join(
      portfolio_data,
      multi_crispy,
      by = portfolio_crispy_merge_cols
    )
  return(analysis_data)
}

#' Compute Analysis Metrics
#'
#' @description Function computing financial metrics to use for analysis
#' @param analysis_data
#'
#' @return
#' @export
#'
#' @examples
compute_analysis_metrics <- function(analysis_data) {
  analysis_data <- analysis_data |>
    dplyr::mutate(
      exposure_at_default = .data$exposure_value_usd * .data$loss_given_default,
      expected_loss_portfolio = .data$exposure_at_default * .data$pd_portfolio,

      # exposure_at_default_baseline = .data$net_present_value_baseline * .data$loss_given_default,
      expected_loss_baseline = .data$exposure_at_default * .data$pd_baseline,

      # exposure_at_default_shock = .data$net_present_value_shock * .data$loss_given_default,
      expected_loss_shock = .data$exposure_at_default * .data$pd_shock,

      pd_difference_shock = .data$pd_portfolio - .data$pd_shock,
      pd_difference_st = .data$pd_baseline - .data$pd_shock
    )

  return(analysis_data)
}


join_trisk_parameters <- function(analysis_data, trisk_runs_params){
  return(dplyr::inner_join(analysis_data, trisk_runs_params, by="run_id"))
}

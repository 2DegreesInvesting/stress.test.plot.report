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
create_analysis_data <-
  function(portfolio_data,
           multi_crispy,
           portfolio_crispy_merge_cols) {
    portfolio_crispy_merge_cols <- setNames(
      paste0("crispy.", portfolio_crispy_merge_cols),
      paste0("portfolio.", portfolio_crispy_merge_cols)
    )

    colnames(portfolio_data) <-
      paste0("portfolio.", colnames(portfolio_data))
    colnames(multi_crispy) <- paste0("crispy.", colnames(multi_crispy))

    analysis_data <-
      dplyr::inner_join(portfolio_data,
                        multi_crispy,
                        by = portfolio_crispy_merge_cols)
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
      net_present_value_difference = .data$crispy.net_present_value_baseline - .data$crispy.net_present_value_shock,

      exposure_at_default = .data$portfolio.exposure_value_usd * .data$portfolio.loss_given_default,
      # exposure_at_default_baseline = .data$net_present_value_baseline * .data$loss_given_default,
      # exposure_at_default_shock = .data$net_present_value_shock * .data$loss_given_default,

      expected_loss_portfolio = .data$exposure_at_default * .data$portfolio.pd_portfolio,
      expected_loss_baseline = .data$exposure_at_default * .data$crispy.pd_baseline,
      expected_loss_shock = .data$exposure_at_default * .data$crispy.pd_shock,

      # pd_difference_portfolio = .data$portfolio.pd_portfolio - .data$crispy.pd_shock,
      pd_difference = .data$crispy.pd_baseline - .data$crispy.pd_shock
    )



  return(analysis_data)
}


join_trisk_parameters <- function(analysis_data, trisk_run_params) {
  colnames(trisk_run_params) <-
    paste0("crispy.", colnames(trisk_run_params))

  analysis_data <- dplyr::inner_join(
    analysis_data,
    trisk_run_params,
    by = c("crispy.run_id")
  )

  return(analysis_data)
}

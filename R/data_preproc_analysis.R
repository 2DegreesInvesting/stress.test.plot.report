create_analysis_data <-
  function(portfolio_data,
           multi_crispy_data,
           portfolio_crispy_merge_cols) {
    # If no portfolio is provided, fill the merging columns
    # with the ones available in crispy, in order to get a full crispy output
    if (nrow(portfolio_data) == 0) {
      merge_cols_values <- multi_crispy_data |>
        dplyr::distinct_at(portfolio_crispy_merge_cols)
      portfolio_data <- dplyr::full_join(portfolio_data, merge_cols_values)
    }

    analysis_data <-
      dplyr::inner_join(
        portfolio_data,
        multi_crispy_data,
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
#'
#' @examples
compute_analysis_metrics <- function(analysis_data) {
  analysis_data <- analysis_data |>
    dplyr::mutate(
      net_present_value_difference = .data$net_present_value_shock - .data$net_present_value_baseline,
      crispy_perc_value_change = .data$net_present_value_difference / .data$net_present_value_baseline,
      crispy_value_loss = .data$crispy_perc_value_change * .data$exposure_value_usd,
      exposure_at_default = .data$exposure_value_usd * .data$loss_given_default,
      # exposure_at_default_baseline = .data$net_present_value_baseline * .data$loss_given_default,
      # exposure_at_default_shock = .data$net_present_value_shock * .data$loss_given_default,

      expected_loss_portfolio = .data$exposure_at_default * .data$pd_portfolio,
      expected_loss_baseline = .data$exposure_at_default * .data$pd_baseline,
      expected_loss_shock = .data$exposure_at_default * .data$pd_shock,

      # pd_difference_portfolio = .data$pd_portfolio - .data$pd_shock,
      pd_difference = .data$pd_shock - .data$pd_baseline,
      crispy_perc_pd_change = .data$pd_difference / .data$pd_baseline
    )



  return(analysis_data)
}

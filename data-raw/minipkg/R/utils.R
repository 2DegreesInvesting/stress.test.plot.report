#' Check that only 1 trisk parameter has been tweaked in the aggregated crispys.
#'
#' @param plot_variable
#'
#' @return
#' @export
#'
#' @examples
is_multivariate_analysis <- function(all_crispy, plot_variable) {

  # TODO trisk_variables should be a global variable
  trisk_variables <-
    c(
      "scenario_geography",
      "risk_free_rate",
      "discount_rate",
      "dividend_rate",
      "growth_rate",
      "shock_year"
    )
  browser()
  trisk_variables_not_for_plot <-
    trisk_variables[!grepl(plot_variable, trisk_variables)]

  unique_param_combinations <- all_crispy %>%
    dplyr::select(dplyr::any_of(trisk_variables_not_for_plot)) %>%
    dplyr::distinct_all()

  return(nrow(unique_param_combinations) == 1)
}

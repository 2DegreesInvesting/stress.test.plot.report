#' Title
#'
#' @param data
#' @param group_variable_charvec
#' @param value_to_plot_char
#'
#' @return
#' @export
#'
#' @examples
prepare_for_pd_annual_plot <- function(data,
                                       group_variable_charvec,
                                       value_to_plot_char) {
  data_pd <- prepare_for_pd_plot(
    analysis_data_single_run, group_variable_charvec,
    value_to_plot_char
  )

  data_pd_annual <- data_pd %>%
    dplyr::mutate(year_date = ymd(.data$shock_year, truncated = 2L))
  data_pd_annual
}

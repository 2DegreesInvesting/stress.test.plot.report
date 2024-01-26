#' Title
#'
#' @description
#' the data in output of this function is NOT aggregated on the group variable
#'
#'
#' @param analysis_data analysis_data
#' @param group_variable_char group_variable_char
#' @param value_to_plot_char value_to_plot_char
#'
#' @export
prepare_for_exposure_plot <- function(analysis_data_single_run, group_variable_char, value_to_plot_char) {
  data_exposure_plot <- analysis_data_single_run |>
    dplyr::mutate(
      group_variable = !!rlang::sym(group_variable_char),
      value_to_plot = !!rlang::sym(value_to_plot_char)
    ) |>
    dplyr::group_by_at(group_variable_char) |>
    dplyr::mutate(
      group_mean = mean(.data$value_to_plot, na.rm = TRUE),
      group_sum = sum(.data$value_to_plot, na.rm = TRUE),
      group_max = max(.data$value_to_plot, na.rm = TRUE)
    )
  data_exposure_plot
}

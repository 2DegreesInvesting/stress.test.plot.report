prepare_for_exposure_plot <- function(analysis_data, group_variable_char, value_to_plot_char) {
  data_exposure_plot <- analysis_data |>
  dplyr::mutate(
    group_variable = !!rlang::sym(group_variable_char),
    value_to_plot = !!rlang::sym(value_to_plot_char)
    ) |>
  dplyr::group_by(.data$group_variable) |>
  dplyr::mutate(
    group_mean = mean(.data$value_to_plot, na.rm = TRUE),
    group_sum = sum(.data$value_to_plot, na.rm = TRUE),
    group_max = max(.data$value_to_plot, na.rm = TRUE)
  )
  data_exposure_plot
}

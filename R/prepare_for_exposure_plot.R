prepare_for_exposure_plot <- function(data) {
  out <- data %>%
  mutate(
    value_to_plot = .data$exposure_at_default,
    group_variable = .data$ald_sector
    ) %>%
  mutate(
    value_to_plot = .data$value_to_plot / 10^6
  ) %>%
  group_by(.data$group_variable) %>%
  mutate(
    group_mean = mean(.data$value_to_plot, na.rm = TRUE),
    group_sum = sum(.data$value_to_plot, na.rm = TRUE),
    group_max = max(.data$value_to_plot, na.rm = TRUE)
  )
  out
}

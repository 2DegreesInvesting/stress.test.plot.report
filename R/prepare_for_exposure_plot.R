prepare_for_exposure_plot <- function(data) {
  out <- data %>%
  filter(
    scenario_name == "Carbon balance 2030",
    scenario_geography == "Global",
    investor_name == "Meta Investor",
    portfolio_name == "Meta Investor"
  ) %>%
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

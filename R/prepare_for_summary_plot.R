prepare_for_summary_plot <- function(data) {
  out <- prepare_for_exposure_plot(data)

  out <- out %>%
  group_by(.data$group_variable) %>%
  summarise(
    group_sum = sum(.data$value_to_plot, na.rm = TRUE),
    group_n = n()
  ) %>%
  pivot_longer(cols = c("group_sum", "group_n")) %>%
  mutate(
    label = case_when(
      .data$name == "group_sum" ~ "Total exposure per sector (Millions)",
      .data$name == "group_n" ~ "Number of companies per sector"
    )
  ) %>%
  group_by(name) %>%
  mutate(
    perc_of_total = .data$value / sum(.data$value)
  )
  out
}

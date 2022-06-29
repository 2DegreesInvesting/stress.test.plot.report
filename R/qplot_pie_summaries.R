qplot_pie_summaries <- function(data) {
  data_summary <- data

  p <- ggplot(
    data,
    aes(x = "", y = .data$perc_of_total, fill = .data$group_variable)
    ) +
  geom_bar(stat = "identity", width = 1) +
  r2dii.colours::scale_fill_2dii("pacta", colour_groups = data_summary$group_variable) +
  coord_polar("y", start = 0) +
  r2dii.plot::theme_2dii() +
  theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(~label) +
  geom_text(aes(x = 1.6, label = glue::glue("{.data$group_variable}:\n{round(.data$value)}")), position = position_stack(vjust = .5)) +
  labs(
    title = "Portfolio composition"
  )
  p
}

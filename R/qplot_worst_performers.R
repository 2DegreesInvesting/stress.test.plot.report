qplot_worst_performers <- function(data) {
  data_worst_performers <- data %>%
  group_by(.data$group_variable) %>%
  slice_max(order_by = .data$value_to_plot, n = 10) %>%
  ungroup() %>%
  mutate(
    company_name = reorder_within(.data$company_name, .data$value_to_plot, .data$group_variable)
    )

  p <- ggplot(data_worst_performers, aes(x = .data$company_name, y = .data$value_to_plot, fill = .data$group_variable)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(.data$value_to_plot, 1)), hjust = -0.2) +
    scale_x_reordered() +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    scale_fill_2dii("pacta", colour_groups = data_worst_performers$group_variable) +
    coord_flip() +
    theme_2dii() +
    theme(
      axis.title.y = element_blank(),
      legend.position = "none"
    ) +
    facet_wrap(~fct_reorder(.data$group_variable, .data$value_to_plot, .fun = max, .desc = TRUE), scales = "free_y", ncol = 1) +
    labs(
      title = "Companies with the highest exposure within sector",
      y = "Exposure (Millions)"
    )
  p
}

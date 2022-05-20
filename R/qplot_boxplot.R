qplot_boxplot <- function(data) {
  data <- remove_outliers(data)
  plot <- ggplot(data, aes(x = .data$group_variable, y = .data$value_to_plot, fill = .data$group_mean)) +
    geom_boxplot() +
    scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
      name = "Average exposure\n per sector (millions)"
    ) +
    theme_2dii() +
    theme(
      legend.title = element_text(),
      strip.text = element_blank()
    ) +
    labs(
      title = "Distributions of exposures per sector",
      subtitle = "Demonstrated using boxplots. The box shows the Interquartile range\n with black thick line showing the median. Black dots represent the outliers.\n The colours represent the average size of the exposure within a data set.",
      x = "Sector",
      y = "Exposure (millions)"
    ) +
    facet_wrap(~.data$group_variable, scales = "free")
  plot
}

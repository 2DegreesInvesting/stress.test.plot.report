qplot_violin <- function(data) {
  data <- remove_outliers(data)
  ggplot(data, aes(x = .data$group_variable, y = .data$value_to_plot, colour = .data$value_to_plot)) +
  geom_violin() +
  geom_jitter(alpha = 0.5) +
  scale_colour_gradient(
    low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
    high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
    name = "Exposure (millions)"
  ) +
  theme_2dii() +
  theme(
    legend.title = element_text(),
    strip.text = element_blank()
  ) +
  labs(
    title = "Distributions of exposures per sector",
    subtitle = "Points represent a company. Position on the y-axis and colour represent\nthe size of the exposure. Curved lines approximate the shape of the distribution.",
    x = "Sector",
    y = "Exposure (millions)"
  ) +
  facet_wrap(~.data$group_variable, scales = "free")
}

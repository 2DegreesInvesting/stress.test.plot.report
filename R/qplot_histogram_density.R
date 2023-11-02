qplot_histogram_density <- function(data_exposure_plot) {
  phist <- ggplot(data_exposure_plot, aes(x = .data$value_to_plot, fill = .data$group_mean)) +
  geom_histogram(bins = 15, stat="identity") +
  scale_y_continuous(
    expand = expansion(mult = c(0, .1)),
                     labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_fill_gradient(
    low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
    high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
    name = "Average exposure\n per sector (millions)"
  ) +
  theme_2dii() +
  theme(
    legend.title = element_text(),
    axis.title.x = element_blank()
  ) +
  labs(
    title = "Distributions of exposures per sector",
    subtitle = "Demonstrated using histograms and density plots.\n The colours represent the average size of the exposure within a data set.",
    x = "Exposure (millions)",
    y = "Number of companies"
  ) +
  facet_wrap(~.data$group_variable, scales = "free")

pdens <- ggplot(data_exposure_plot, aes(x = .data$value_to_plot, fill = .data$group_mean)) +
  geom_density(aes(y = ..scaled..)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
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
    x = "Exposure (millions)",
    y = "Density"
  ) +
  facet_wrap(~.data$group_variable, scales = "free")

(phist / pdens + patchwork::plot_layout(guides = "collect"))
}

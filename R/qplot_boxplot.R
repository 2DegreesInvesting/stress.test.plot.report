#' Title
#'
#' @param data_exposure_plot
#'
#' @return
#' @export
#'
#' @examples
qplot_boxplot <- function(data_exposure_plot) {
  data_exposure_plot <- remove_outliers(data_exposure_plot)
  plot <- ggplot(data_exposure_plot, aes(x = .data$group_variable, y = .data$value_to_plot, fill = .data$group_mean)) +
    geom_boxplot() +
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
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
    facet_wrap(~ .data$group_variable, scales = "free")
  plot
}

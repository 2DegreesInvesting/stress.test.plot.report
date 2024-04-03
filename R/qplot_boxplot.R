#' Title
#'
#' @param data_exposure_plot data_exposure_plot
#'
#' @export
qplot_boxplot <- function(data_exposure_plot) {
  data_exposure_plot <- remove_outliers_legacy(data_exposure_plot)
  plot <- ggplot2::ggplot(data_exposure_plot,
                          ggplot2::aes(x = .data$group_variable, y = .data$value_to_plot, fill = .data$group_mean)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    ggplot2::scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
      name = "Average exposure\n per sector (millions)"
    ) +
    r2dii.plot::theme_2dii() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(),
      strip.text = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Distributions of exposures per sector",
      subtitle = "Demonstrated using boxplots. The box shows the Interquartile range\n with black thick line showing the median. Black dots represent the outliers.\n The colours represent the average size of the exposure within a data set.",
      x = "Sector",
      y = "Exposure (millions)"
    ) +
    ggplot2::facet_wrap(~ .data$group_variable, scales = "free")
  plot
}

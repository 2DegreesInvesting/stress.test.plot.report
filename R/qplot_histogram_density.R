#' Title
#'
#' @param data_exposure_plot
#' @param plot_variable
#'
#' @return
#' @export
#'
#' @examples
qplot_histogram_density <- function(data_exposure_plot, plot_variable) {
  phist <-
    ggplot(data_exposure_plot,
           aes(x = .data$value_to_plot,  fill = .data$group_mean)) +
    ggplot2::geom_histogram(bins = 15) +
    ggplot2::scale_y_continuous(
      expand = expansion(mult = c(0, .1))
    ) +
    ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    ggplot2::scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
      name = "Average exposure\n per sector (millions)"
    ) +
    r2dii.plot::theme_2dii() +
    ggplot2::theme(legend.title = element_text(),
          axis.title.x = element_blank()) +
    ggplot2::labs(
      title = "Distributions of exposures per sector",
      subtitle = "Demonstrated using histograms and density plots.\n The colours represent the average size of the exposure within a data set.",
      x = "Exposure (millions)",
      y = "Number of companies"
    ) +
    ggplot2::facet_wrap( ~ .data$group_variable, scales = "free")+
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust=1))

  pdens <-
    ggplot2::ggplot(data_exposure_plot,
           aes(x = .data$value_to_plot, fill = .data$group_mean)) +
    ggplot2::geom_density(aes(y = ..scaled..)) +
    ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    ggplot2::scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    ggplot2::scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
      name = "Average exposure\n per sector (millions)"
    ) +
    r2dii.plot::theme_2dii() +
    ggplot2::theme(legend.title = element_text(),
          strip.text = element_blank()) +
    ggplot2::labs(x = "Exposure (millions)",
         y = "Density") +
    ggplot2::facet_wrap( ~ .data$group_variable, scales = "free")+
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust=1))

  (phist / pdens) +
    patchwork::plot_layout(guides = "collect")
}

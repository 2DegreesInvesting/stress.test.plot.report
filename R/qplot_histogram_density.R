#' Title
#'
#' @param data_exposure_plot
#'
#' @return
#' @export
#'
#' @examples
qplot_histogram_density <- function(data_exposure_plot) {
  phist <-
    ggplot2::ggplot(
      data_exposure_plot,
      ggplot2::aes(x = .data$value_to_plot, fill = .data$group_mean)
    ) +
    ggplot2::geom_histogram(bins = 15) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, .1))
    ) +
    ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    ggplot2::scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "grey") |> dplyr::pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "red") |> dplyr::pull(.data$hex),
      name = "Average exposure\n per sector (millions)"
    ) +
    r2dii.plot::theme_2dii() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(),
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Distributions of exposures per sector",
      subtitle = "Demonstrated using histograms and density plots.\n The colours represent the average size of the exposure within a data set.",
      x = "Exposure (millions)",
      y = "Number of companies"
    ) +
    ggplot2::facet_wrap(~ .data$group_variable, scales = "free") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  pdens <-
    ggplot2::ggplot(
      data_exposure_plot,
      ggplot2::aes(x = .data$value_to_plot, fill = .data$group_mean)
    ) +
    ggplot2::geom_density(ggplot2::aes(y = ..scaled..)) +
    ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "grey") |> dplyr::pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "red") |> dplyr::pull(.data$hex),
      name = "Average exposure\n per sector (millions)"
    ) +
    r2dii.plot::theme_2dii() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(),
      strip.text = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Exposure (millions)",
      y = "Density"
    ) +
    ggplot2::facet_wrap(~ .data$group_variable, scales = "free") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  gridExtra::grid.arrange(phist, pdens, ncol = 1)
}

#' Title
#'
#' @param data_exposure_plot
#'
#' @return
#' @export
#'
#' @examples
qplot_violin <- function(data_exposure_plot) {
  data_exposure_plot_no_outlier <- remove_outliers_legacy(data_exposure_plot)
  ggplot2::ggplot(data_exposure_plot_no_outlier, aes(x = .data$group_variable, y = .data$value_to_plot, colour = .data$value_to_plot)) +
    ggplot2::geom_violin() +
    ggplot2::geom_jitter(alpha = 0.5) +
    ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    ggplot2::scale_color_gradient(
      low = r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "grey") |> dplyr::pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "red") |> dplyr::pull(.data$hex),
      name = "Exposure (millions)"
    ) +
    r2dii.plot::theme_2dii() +
    ggplot2::theme(
      legend.title = element_text(),
      strip.text = element_blank()
    ) +
    ggplot2::labs(
      title = "Distributions of exposures per sector",
      subtitle = "Points represent a company. Position on the y-axis and colour represent\nthe size of the exposure. Curved lines approximate the shape of the distribution.",
      x = "Sector",
      y = "Exposure (millions)"
    ) +
    ggplot2::facet_wrap(~ .data$group_variable, scales = "free")
}

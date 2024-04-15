#' Title
#'
#' @param data_pd data_pd
#' @param common_y_axis common_y_axis
#' @param annual_pds annual_pds
#'
#' @export
#'
qplot_pd_sector <- function(data_pd, common_y_axis = FALSE, annual_pds = FALSE) {
  if (!annual_pds) {
    p <- ggplot2::ggplot(data_pd, ggplot2::aes(x = term, y = .data$value_to_plot, fill = .data$value_to_plot))
  } else {
    p <- ggplot2::ggplot(data_pd, ggplot2::aes(x = shock_year, y = .data$value_to_plot, fill = .data$value_to_plot))
  }
  p <- p +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot %>% dplyr::filter(.data$label == "red") %>% dplyr::pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>% dplyr::filter(.data$label == "grey") %>% dplyr::pull(.data$hex),
      name = "PD change (% points)"
    ) +
    r2dii.plot::theme_2dii() +
    ggplot2::theme(
      legend.title = ggplot2::element_text()
    )

  if (common_y_axis) {
    p <- p + ggplot2::facet_wrap(~ald_sector, scales = "fixed")
  } else {
    p <- p + ggplot2::facet_grid(~ald_sector, scales = "free")
  }

  if (!annual_pds) {
    p <- p +
      ggplot2::labs(
        title = "Overall PD change by sector and shock year",
        x = "Term",
        y = "PD change (% points)"
      )
  } else {
    ggplot2::labs(
      title = "Annual PD changes per sector and shock year",
      x = "Year",
      y = "PD change (% points)"
    )
  }
  p
}

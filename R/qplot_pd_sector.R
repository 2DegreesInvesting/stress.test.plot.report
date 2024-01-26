#' Title
#'
#' @param data_pd
#' @param common_y_axis
#' @param annual_pds
#'
#' @return
#' @export
#'
#' @examples
qplot_pd_sector <- function(data_pd, common_y_axis = FALSE, annual_pds = FALSE) {
  if (!annual_pds) {
    p <- ggplot(data_pd, aes(x = term, y = .data$value_to_plot, fill = .data$value_to_plot))
  } else {
    p <- ggplot(data_pd, aes(x = shock_year, y = .data$value_to_plot, fill = .data$value_to_plot))
  }
  p <- p +
    geom_bar(stat = "identity") +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
      name = "PD change (% points)"
    ) +
    theme_2dii() +
    theme(
      legend.title = element_text()
    )

  if (common_y_axis) {
    p <- p + facet_wrap(~ald_sector, scales = "fixed")
  } else {
    p <- p + facet_grid(~ald_sector, scales = "free")
  }

  if (!annual_pds) {
    p <- p +
      labs(
        title = "Overall PD change by sector and shock year",
        x = "Term",
        y = "PD change (% points)"
      )
  } else {
    labs(
      title = "Annual PD changes per sector and shock year",
      x = "Year",
      y = "PD change (% points)"
    )
  }
  p
}

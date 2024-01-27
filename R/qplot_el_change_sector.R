#' Title
#'
#' @param data_el_plot data_el_plot
#'
#' @export
qplot_el_change_sector <- function(data_el_plot) {
  data_change <- data_el_plot %>%
    filter(expected_loss_type %in% c("st_diff"))

  p_abs <- qplot_el_change_sector_abs(data_change)

  p_perc <- qplot_el_change_sector_perc(data_change)

  gridExtra::grid.arrange(p_perc, p_abs, ncol = 1)
}

#' Title
#'
#' @param data_change data_change
#'
#' @export
qplot_el_change_sector_perc <- function(data_change) {
  p <- plot_el_coloured(data_change, "el_as_perc_exposure", is_percentage = TRUE) +
    facet_wrap(~group_variable, scales = "fixed") +
    labs(
      title = "Expected loss shock as percentage of exposure per sector",
      y = "Expected Loss\n(% exposure)"
    )
  p
}

#' Title
#'
#' @param data_change data_change
#'
#' @export
qplot_el_change_sector_abs <- function(data_change) {
  p <- plot_el_coloured(data_change, "expected_loss_value") +
    facet_wrap(~group_variable, scales = "fixed") +
    labs(
      title = "Expected loss shock per sector",
      y = "Expected Loss\n(currency)"
    )
  p
}

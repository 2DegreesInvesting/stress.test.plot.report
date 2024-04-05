#' Title
#'
#' @param data data
#'
#' @export
qplot_val_change_technology <- function(data) {
  p_perc <- qplot_val_change_technology_perc(data)

  p_abs <- qplot_val_change_technology_abs(data)
  p <- p_perc / p_abs
  p
}

#' Title
#'
#' @param data data
#'
#' @export
qplot_val_change_technology_perc <- function(data) {
  p <- plot_value_change_coloured(data, "technology_percent_value_change") +
    ggplot2::facet_wrap(ald_sector ~ technology) +
    ggplot2::labs(
      title = "Percentage value change per technology",
      y = "Value change\n(% points)"
    )
  p
}

#' Title
#'
#' @param data
#'
#' @export
qplot_val_change_technology_abs <- function(data) {
  p <- plot_value_change_coloured(data, "technology_absolute_value_change") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::facet_wrap(ald_sector ~ technology) +
    ggplot2::labs(
      title = "Absolute value change per technology",
      y = "Value change\n(currency)"
    )
  p
}

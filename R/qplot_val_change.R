#' Title
#'
#' @param data_val_change data_val_change
#'
#' @export
#'
qplot_val_change <- function(data_val_change) {
  # prepare data for plots
  group_cols <-
    colnames(data_val_change)[!colnames(data_val_change) %in% c("value_change_absolute", "value_change_percent")]
  data_plt <- data_val_change |>
    tidyr::unite("xaxis_values",
      all_of(group_cols),
      sep = "-",
      remove = TRUE
    )

  # do plots
  p_perc <-
    plot_value_change(data_plt,
      is_percentage = TRUE
    ) +
    ggplot2::labs(
      title = "Analysed portfolio percentage value change",
      y = "Value change (% points)"
    )

  p_abs <- plot_value_change(data_plt,
    is_percentage = FALSE
  ) +
    ggplot2::labs(
      title = "Analysed portfolio absolute value change",
      y = "Value change (currency)"
    )

  gridExtra::grid.arrange(p_perc + patchwork::plot_spacer(), p_abs + patchwork::plot_spacer(), ncol = 1)
}

#' Title
#'
#' @param data_plt data_plt
#' @param is_percentage is_percentage
#'
#' @export
#'
plot_value_change <- function(data_plt, is_percentage) {
  if (is_percentage) {
    labels <- scales::percent
    y_val_name <- "value_change_percent"
  } else {
    labels <- scales::unit_format(unit = "M", scale = 1e-6)
    y_val_name <- "value_change_absolute"
  }

  p <-
    ggplot2::ggplot(
      data_plt,
      ggplot2::aes(x = xaxis_values, y = !!rlang::sym(y_val_name), fill = !!rlang::sym(y_val_name))
    ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = 0) +
    r2dii.plot::theme_2dii() +
    ggplot2::theme(
      legend.position = "right",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      # axis.ticks.x = element_blank(),
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot %>%
        dplyr::filter(.data$label == "red") %>%
        dplyr::pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>%
        dplyr::filter(.data$label == "green") %>%
        dplyr::pull(.data$hex),
      # midpoint = min(data_plt[,y_val_name]),
      labels = labels,
      name = "Expected loss"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.1, 0)), labels = labels)

  p
}

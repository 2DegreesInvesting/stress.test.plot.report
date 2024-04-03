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
    labs(
      title = "Analysed portfolio percentage value change",
      y = "Value change (% points)"
    )

  p_abs <- plot_value_change(data_plt,
    is_percentage = FALSE
  ) +
    labs(
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
    ggplot(
      data_plt,
      aes(x = xaxis_values, y = !!rlang::sym(y_val_name), fill = !!rlang::sym(y_val_name))
    ) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0) +
    theme_2dii() +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      # axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ) +
    scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot %>%
        filter(.data$label == "red") %>%
        pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>%
        filter(.data$label == "green") %>%
        pull(.data$hex),
      # midpoint = min(data_plt[,y_val_name]),
      labels = labels,
      name = "Expected loss"
    ) +
    scale_y_continuous(expand = expansion(mult = c(.1, 0)), labels = labels)

  p
}

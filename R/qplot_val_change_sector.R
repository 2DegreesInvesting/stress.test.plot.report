#' Title
#'
#' @param data_val_change The data frame containing the value change data.
#' @param facet_chr The column name to be used for faceting the plots.
#'
#' @return None
#' @export
#'
#' @examples
qplot_val_change_facet <- function(data_val_change, facet_chr) {
  # prepare data for plots
  group_cols <-
    colnames(data_val_change)[!colnames(data_val_change) %in% c("ald_sector", "value_change_absolute", "value_change_percent")]
  data_plt <- data_val_change |>
    tidyr::unite("xaxis_values",
      all_of(group_cols),
      sep = "-",
      remove = TRUE
    )


  # do plots
  p_perc <- plot_value_change_coloured(data_plt, is_percentage = TRUE) +
    facet_wrap(as.formula(paste("~", facet_chr))) +
    labs(
      title = "Percentage value change per sector",
      y = "Value change\n(% points)"
    )

  p_abs <- plot_value_change_coloured(data_plt, is_percentage = FALSE) +
    facet_wrap(as.formula(paste("~", facet_chr))) +
    labs(
      title = "Absolute value change per sector",
      y = "Value change\n(currency)"
    )

  gridExtra::grid.arrange(p_perc + patchwork::plot_spacer(), p_abs + patchwork::plot_spacer(), ncol = 1)
}

#' Title
#'
#' @param data The data frame containing the value change data.
#' @param is_percentage A logical value indicating whether the value change is in percentage or absolute.
#'
#' @return None
#' @export
plot_value_change_coloured <- function(data, is_percentage) {
  if (is_percentage) {
    labels <- scales::percent
    y_val_name <- "value_change_percent"
  } else {
    labels <- scales::unit_format(unit = "M", scale = 1e-6)
    y_val_name <- "value_change_absolute"
  }


  p <- ggplot(
    data,
    aes(
      x = xaxis_values,
      y = !!rlang::sym(y_val_name),
      fill = !!rlang::sym(y_val_name)
    )
  ) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0) +
    scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "green") %>% pull(.data$hex),
      labels = labels,
      name = "Equity value change"
    ) +
    theme_2dii() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      # axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      # axis.line.x = element_blank(),
      legend.title = element_text()
    ) +
    scale_y_continuous(expand = expansion(mult = c(.1, 0)), labels = labels)
}

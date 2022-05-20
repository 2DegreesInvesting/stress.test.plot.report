qplot_val_change_technology <- function(data) {
    p_perc <- plot_value_change_coloured(data, "technology_percent_value_change") +
      facet_wrap(ald_sector ~ technology) +
      labs(
        title = "Percentage value change per technology",
        y = "Value change\n(% points)"
      )

    p_abs <- plot_value_change_coloured(data, "technology_absolute_value_change") +
      scale_y_continuous(labels = scales::comma) +
      facet_wrap(ald_sector ~ technology) +
      labs(
        title = "Absolute value change per technology",
        y = "Value change\n(currency)"
      )
    p <- p_perc / p_abs
    p
}

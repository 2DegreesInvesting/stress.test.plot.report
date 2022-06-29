qplot_val_change_technology <- function(data) {
  p_perc <- qplot_val_change_technology_perc(data)

  p_abs <- qplot_val_change_technology_abs(data)
  p <- p_perc / p_abs
  p
}

qplot_val_change_technology_perc <- function(data) {
  p <- plot_value_change_coloured(data, "technology_percent_value_change") +
    facet_grid(~ ald_sector + technology) +
    labs(
      title = "Percentage value change per technology",
      y = "Value change (%)"
    ) +
    theme(strip.text.x = element_blank())
  p
}

qplot_val_change_technology_abs <- function(data) {
  p <- plot_value_change_coloured(data, "technology_absolute_value_change") +
    scale_y_continuous(labels = scales::comma) +
    facet_grid(~ ald_sector + technology, switch = "x") +
    labs(
      title = "Absolute value change per technology",
      y = "Value change\n(currency)"
    )
  p
}

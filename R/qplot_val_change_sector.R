qplot_val_change_sector <- function(data) {
    p_perc <- qplot_val_change_sector_perc(data)

    p_abs <- qplot_val_change_sector_abs(data)
    p <- p_perc / p_abs
    p
}

plot_value_change_coloured <- function(data, y_val_name) {
  p <- ggplot(
    data,
    aes_string(
      x = "portfolio_name",
      y = y_val_name,
      fill = y_val_name
      )
    ) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  scale_fill_gradient2(
      low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "green") %>% pull(.data$hex),
      midpoint = 0,
      labels = scales::comma,
      name = "Equity value change"
    ) +

  theme_2dii() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    legend.title = element_text()
  )
}

qplot_val_change_sector_perc <- function(data) {
  data_plt <- data %>%
      group_by(ald_sector) %>%
      select(portfolio_name, sector_percent_value_change, sector_absolute_value_change, ald_sector) %>%
      distinct()

  p <- plot_value_change_coloured(data_plt, "sector_percent_value_change") +
      facet_wrap(ald_sector ~ .) +
      labs(
        title = "Percentage value change per sector",
        y = "Value change (%)"
      )
  p
}

qplot_val_change_sector_abs <- function(data) {
  data_plt <- data %>%
      group_by(ald_sector) %>%
      select(portfolio_name, sector_percent_value_change, sector_absolute_value_change, ald_sector) %>%
      distinct()

  p <- plot_value_change_coloured(data_plt, "sector_absolute_value_change") +
      scale_y_continuous(labels = scales::comma) +
      facet_wrap(ald_sector ~ .) +
      labs(
        title = "Absolute value change per sector",
        y = "Value change\n(currency)"
      )
  p
}

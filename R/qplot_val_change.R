qplot_val_change <- function(data) {
  data_plt <- data %>%
    group_by(portfolio_name) %>%
    select(portfolio_name, analysed_portfolio_percent_value_change, analysed_portfolio_absolute_value_change) %>%
    distinct()

  p_perc <- plot_value_change(
    data_plt,
    y_val_name = "analysed_portfolio_percent_value_change"
    ) +
  labs(
    title = "Analysed portfolio percentage value change",
    y = "Value change (% points)"
  )

  p_abs <- plot_value_change(
    data_plt,
    y_val_name = "analysed_portfolio_absolute_value_change"
    ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Analysed portfolio absolute value change",
    y = "Value change (currency)"
  )

  p <- (p_perc + patchwork::plot_spacer())/ (p_abs + patchwork::plot_spacer())
  p
}

plot_value_change <- function(data, y_val_name) {
  p <- ggplot(data, aes_string(x = "portfolio_name", y = y_val_name)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  theme_2dii() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )
  p
}

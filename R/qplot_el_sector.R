qplot_el_sector <- function(data) {
  data_losses <- data %>%
    filter(name %in% c("expected_loss_baseline", "expected_loss_shock")) %>%
    mutate(
      name = str_replace(name, "expected_loss_", "")
    )

  p_abs <- plot_el_coloured(data_losses, "value") +
    facet_wrap(~ ald_sector, scales = "free") +
    labs(
      title = "Expected loss per sector",
      subtitle = "For baseline and shock scenarios",
      y = "Expected Loss (currency)"
    )

  p_perc <- plot_el_coloured(data_losses, "el_as_perc_exposure", is_percentage = TRUE) +
    facet_wrap(~ ald_sector, scales = "free") +
    labs(
      title = "Expected Loss as percentage of exposure per sector",
      subtitle = "For baseline and shock scenarios",
      y = "Expected Loss (% exposure)"
    )

  p_perc / p_abs
}

plot_el_coloured <- function(data, y_val_name, is_percentage = FALSE) {
  if(is_percentage) {
    labels <- scales::percent
  } else {
    labels <- scales::comma
  }

  p <- ggplot(
    data,
    aes_string(x = "name", y = y_val_name, fill = y_val_name)
    ) +
    geom_bar(stat = "identity", color = "grey") +
    scale_x_discrete(position = "top", labels = r2dii.plot::to_title) +
    scale_y_continuous(expand = expansion(mult = c(.1, 0)), labels = labels) +
    scale_fill_gradient2(
        low = r2dii.colours::palette_1in1000_plot %>%
          filter(.data$label == "red") %>%
          pull(.data$hex),
        high = r2dii.colours::palette_1in1000_plot %>%
          filter(.data$label == "green") %>%
          pull(.data$hex),
        midpoint = 0,
        labels = labels,
        name = "Expected loss"
      ) +
    theme_2dii() +
    theme(
      legend.title = element_text(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      strip.placement = "outside"
    )
  p
}

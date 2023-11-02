#' Title
#'
#' @param data_el_plot
#'
#' @return
#' @export
#'
#' @examples
qplot_el_sector <- function(data_el_plot) {
  data_losses <- data_el_plot %>%
    filter(expected_loss_type %in% c("baseline", "shock"))

  p_abs <- plot_el_coloured(data_losses, "expected_loss_value") +
    facet_wrap(~ group_variable, scales = "free") +
    labs(
      title = "Expected loss per sector",
      subtitle = "For baseline and shock scenarios",
      y = "Expected Loss (currency)"
    )

  p_perc <- plot_el_coloured(data_losses, "el_as_perc_exposure", is_percentage = TRUE) +
    facet_wrap(~ group_variable, scales = "free") +
    labs(
      title = "Expected Loss as percentage of exposure per sector",
      subtitle = "For baseline and shock scenarios",
      y = "Expected Loss (% exposure)"
    )

  p_perc / p_abs
}

#' Title
#'
#' @param data
#' @param y_val_name
#' @param is_percentage
#'
#' @return
#'
#' @examples
plot_el_coloured <- function(data, y_val_name, is_percentage = FALSE) {
  if(is_percentage) {
    labels <- scales::percent
  } else {
    labels <- scales::comma
  }

  p <- ggplot(
    data,
    aes_string(x = "expected_loss_type", y = y_val_name, fill = y_val_name)
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

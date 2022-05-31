qplot_el_change_sector <- function(data) {
  data_change <- data %>%
    filter(name %in% c("value_change"))

  p_abs <- qplot_el_change_sector_abs(data)

  p_perc <- qplot_el_change_sector_perc(data)

  p_perc / p_abs
}

qplot_el_change_sector_perc <- function(data) {
  data_change <- data %>%
    filter(name %in% c("value_change"))

  p <- plot_el_coloured(data_change, "el_as_perc_exposure", is_percentage = TRUE) +
    facet_wrap(~ ald_sector, scales = "free") +
    labs(
      title = "Expected loss shock as percentage of exposure per sector",
      y = "Expected Loss\n(% exposure)"
    )
  p
}

qplot_el_change_sector_abs <- function(data) {
  data_change <- data %>%
    filter(name %in% c("value_change"))

  p <- plot_el_coloured(data_change, "value") +
    facet_wrap(~ ald_sector, scales = "free") +
    labs(
      title = "Expected loss shock per sector",
      y = "Expected Loss\n(currency)"
    )
  p
}

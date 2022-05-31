qplot_pd_sector <- function(data, common_y_axis = FALSE, annual_pds = FALSE) {
  if (!annual_pds) {
    p <- ggplot(data, aes(x = term, y = pd_change_sector_shock, fill = pd_change_sector_shock))
  } else {
    p <- ggplot(data, aes(x = year_date, y = pd_change_sector_shock, fill = pd_change_sector_shock))
  }
  p <- p +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  scale_fill_gradient(
      low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
      high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
      name = "PD change (% points)"
    ) +
  theme_2dii() +
  theme(
    legend.title = element_text()
  )

  if (!common_y_axis) {
    p <- p + facet_grid(shock_year_arg ~  ald_sector, scales = "free")
  } else {
    p <- p + facet_wrap(ald_sector ~ shock_year_arg, scales = "free")
  }

  if (!annual_pds) {
    p <- p +
    labs(
      title = "Overall PD change by sector and shock year",
      x = "Maturity (years)",
      y = "PD change (% points)"
    )
  } else {
    labs(
      title = "Annual PD changes per sector and shock year",
      x = "Year",
      y = "PD change (% points)"
    )
  }
  p
}

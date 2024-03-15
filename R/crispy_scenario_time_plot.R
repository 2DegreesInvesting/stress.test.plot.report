box::use(
  dplyr[select_at],
  ggplot2[
    ggplot, geom_line, geom_point, facet_wrap, theme_minimal, labs, aes,
    scale_color_manual, theme, element_line, scale_y_continuous
  ],
  dplyr[select_at],
  ggrepel[geom_text_repel]
)

pipeline_scenario_time_plot <- function(
    scenario_data,
    x_var = "year",
    y_var,
    facet_var = c("ald_sector", "ald_business_unit")) {
  facets_colors <- r2dii.colours::palette_2dii_plot[seq_along(unique(scenario_data[[facet_var[1]]])), ]$hex

  data_scenario_time_plot <- prepare_for_scenario_time_plot(scenario_data, x_var, y_var, facet_var)

  scenario_time_plot <- draw_scenario_time_plot(
    data_scenario_time_plot,
    x_var = x_var, y_var = y_var, facet_var = facet_var, facets_colors = facets_colors
  )

  return(scenario_time_plot)
}

prepare_for_scenario_time_plot <- function(scenario_data, x_var, y_var, facet_var) {
  data_scenario_time_plot <- scenario_data |>
    select_at(
      c(x_var, y_var, facet_var)
    )
  return(data_scenario_time_plot)
}


draw_scenario_time_plot <- function(
    data_scenario_time_plot,
    x_var,
    y_var,
    facet_var,
    facets_colors) {
  scenario_time_plot <- ggplot(data_scenario_time_plot, aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var), color = !!rlang::sym(facet_var[1]))) +
    geom_line() +
    geom_point() +
    facet_wrap(stats::as.formula(paste("~", paste(facet_var, collapse = "+"))), scales = "fixed", ncol = 2) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = facets_colors) +
    r2dii.plot::theme_2dii() +
    theme(panel.grid.major.y = element_line(size = .1, color = "black")) +
    labs(
      x = "Year",
      y = "Production as a percentage of the maximum"
    )


  return(scenario_time_plot)
}

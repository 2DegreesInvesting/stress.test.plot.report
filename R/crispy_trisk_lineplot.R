box::use(
  ggplot2[ggplot, aes],
  ggrepel[geom_text_repel]
)


pipeline_crispy_trisk_line_plot <- function(
    trajectories_data,
    x_var = "year",
    facet_var = "ald_business_unit",
    linecolor = "ald_sector") {
  linecolor <- dplyr::intersect(colnames(trajectories_data), linecolor)

  data_trisk_line_plot <- prepare_for_trisk_line_plot(
    trajectories_data = trajectories_data,
    facet_var = facet_var,
    linecolor = linecolor
  )

  trisk_line_plot <- draw_trisk_line_plot(
    data_trisk_line_plot,
    x_var = x_var,
    facet_var = facet_var,
    linecolor = linecolor
  )

  return(trisk_line_plot)
}

prepare_for_trisk_line_plot <- function(trajectories_data, facet_var, linecolor) {
  # Preprocessing to calculate percentages of the maximum value
  data_trisk_line_plot <- trajectories_data |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("production_"),
      names_to = "scenario",
      values_to = "production"
    ) |>
    dplyr::group_by_at(c("year", linecolor, facet_var)) |>
    dplyr::mutate(
      max_production = max(production, na.rm = TRUE),
      production_pct = production/max_production * 100
    ) |>
    dplyr::ungroup()

  return(data_trisk_line_plot)
}


draw_trisk_line_plot <- function(
    data_trisk_line_plot,
    x_var,
    facet_var,
    linecolor) {
  facets_colors <- r2dii.colours::palette_2dii_plot[seq_along(unique(data_trisk_line_plot[[linecolor]])), ]$hex
  trisk_line_plot <- ggplot(
    data_trisk_line_plot,
    aes(
      x = !!rlang::sym(x_var),
      y = production_pct,
      color = !!rlang::sym(linecolor),
      linetype = scenario
    )
  ) +
    ggplot2::geom_line() +
    # ggplot2::geom_point(size = 0.1) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    ggplot2::scale_linetype_manual(values = c(
      "production_baseline_scenario" = "dotted",
      "production_target_scenario" = "dashed",
      "production_shock_scenario" = "solid"
    )) +
    ggplot2::labs(
      x = "Year",
      y = "Production as a percentage of the maximum",
      linetype = "Scenario"
    ) +
    ggplot2::scale_color_manual(values = facets_colors) +
    r2dii.plot::theme_2dii() +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::facet_wrap(
      stats::as.formula(paste("~", paste(facet_var, collapse = "+"))),
      scales = "free_y",
      ncol = 1
    )


  return(trisk_line_plot)
}

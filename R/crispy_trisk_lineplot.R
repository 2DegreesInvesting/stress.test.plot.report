#' Visualize Trajectory Risks Over Time by Business Unit and Sector
#'
#' This function generates a line plot visualizing the trajectory of risks over time, segmented by business units and differentiated by sectors. It allows stakeholders to assess trends and shifts in risk levels across different scenarios, facilitating informed decision-making.
#'
#' @param trajectories_data Dataframe containing yearly data on risk trajectories across different business units and sectors.
#' @param x_var The time variable, defaulting to "year".
#' @param facet_var The variable for faceting the plot by business units, defaulting to "ald_business_unit".
#' @param linecolor Variable determining line colors by sector, defaulting to "ald_sector".
#'
#' @return A ggplot object displaying the trend of risk trajectories over time, providing insights into risk management and strategic planning.
#' @export
pipeline_crispy_trisk_line_plot <- function(
    trajectories_data,
    x_var = "year",
    facet_var = "ald_business_unit",
    linecolor = "ald_sector",
    y_in_percent=TRUE) {
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
    linecolor = linecolor,
    y_in_percent=y_in_percent
  )

  return(trisk_line_plot)
}


#' Prepare Data for Risk Trajectory Line Plot
#'
#' Transforms trajectory data for line plot visualization, calculating production percentages of the maximum value for a clearer comparison across scenarios. This preprocessing is crucial for highlighting relative changes and trends in production risk.
#'
#' @param trajectories_data Dataset containing trajectory information across different scenarios.
#' @param facet_var Variable for faceting plots by business units.
#' @param linecolor Variable for coloring lines by sector.
#'
#' @return A dataframe ready for plotting, with production percentages and scenario data for visualization.
prepare_for_trisk_line_plot <- function(trajectories_data, facet_var, linecolor) {
  # Preprocessing to calculate percentages of the maximum value
  data_trisk_line_plot <- trajectories_data |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("production_"),
      names_to = "scenario",
      values_to = "production"
    ) |>
    dplyr::group_by_at(c(facet_var, linecolor, "scenario", "year")) |>
    dplyr::summarise(
      production = sum(production)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by_at(c(facet_var, linecolor, "scenario")) |>
    dplyr::mutate(
      base_year_production = dplyr::first(production),
      production_pct = production / base_year_production
    ) |>
    # Filter to remove last year of each group
    dplyr::group_by_at(c(facet_var, linecolor, "scenario")) |>
    dplyr::filter(year != max(year)) |>
    dplyr::select(-base_year_production)

  return(data_trisk_line_plot)
}


#' Draw Line Plot for Risk Trajectories
#'
#' Creates a line plot to depict the production risk trajectories as a percentage of the maximum value, offering a visual comparison across different scenarios within business units and sectors. Uses color and linetype variations to differentiate between sectors and scenarios.
#'
#' @param data_trisk_line_plot Prepared data for plotting, with production percentages and scenarios.
#' @param x_var Time variable for the x-axis.
#' @param facet_var Variable for faceting plots by business units.
#' @param linecolor Variable for coloring lines by sector.
#'
#' @return A ggplot object illustrating risk trajectories over time, aiding in the analysis of production risk and scenario planning.
draw_trisk_line_plot <- function(
    data_trisk_line_plot,
    x_var,
    facet_var,
    linecolor,
    y_in_percent) {

  if (y_in_percent){
    trisk_line_plot <- ggplot2::ggplot(
      data_trisk_line_plot,
      ggplot2::aes(
        x = !!rlang::sym(x_var),
        y = production_pct,
        color = !!rlang::sym(linecolor),
        linetype = scenario
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      ggplot2::labs(
        y = "Production as a percentage of the maximum"
      )
  } else {
    trisk_line_plot <- ggplot2::ggplot(
      data_trisk_line_plot,
      ggplot2::aes(
        x = !!rlang::sym(x_var),
        y = production,
        color = !!rlang::sym(linecolor),
        linetype = scenario
      )
    ) +
    ggplot2::scale_y_continuous(labels = function(x) scales::scientific(x)) +
    ggplot2::labs(
      y = "Production in raw unit"
      )
  }


  facets_colors <- r2dii.colours::palette_2dii_plot[seq_along(unique(data_trisk_line_plot[[linecolor]])), ]$hex
  trisk_line_plot <- trisk_line_plot +
    ggplot2::geom_line() +
    # ggplot2::geom_point(size = 0.1) +
    ggplot2::scale_linetype_manual(values = c(
      "production_baseline_scenario" = "dotted",
      "production_target_scenario" = "dashed",
      "production_shock_scenario" = "solid"
    )) +
    ggplot2::labs(
      x = "Year",
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

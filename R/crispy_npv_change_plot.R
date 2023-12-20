box::use(
  dplyr[select_at],
  ggplot2[
    ggplot, aes, geom_col, geom_tile, scale_fill_gradient2, labs, theme, element_text,
    scale_x_discrete, scale_y_continuous, element_blank
  ]
)

pipeline_crispy_npv_change_plot <- function(
    analysis_data,
    x_var = "portfolio.ald_sector",
    y_var = "crispy_perc_value_change") {
  plot_color_gradient <- c(
    r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "red") |> dplyr::pull(.data$hex),
    r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "grey") |> dplyr::pull(.data$hex),
    r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "green") |> dplyr::pull(.data$hex)
  )


  data_crispy_npv_change_plot <- prepare_for_crispy_npv_change_plot(analysis_data, x_var, y_var)

  crispy_npv_change_plot <- draw_crispy_npv_change_plot(
    data_crispy_npv_change_plot,
    x_var, y_var, plot_color_gradient
  )

  return(crispy_npv_change_plot)
}

prepare_for_crispy_npv_change_plot <- function(analysis_data, x_var, y_var) {
  data_exposure_change <- analysis_data |>
    select_at(
      c(x_var, y_var)
    )
  return(data_exposure_change)
}


draw_crispy_npv_change_plot <- function(
    data_crispy_npv_change_plot,
    x_var,
    y_var,
    plot_color_gradient) {
  bar_width <- 0.9 # Adjust as needed TODO variabiliser conf

  crispy_npv_change_plot <- ggplot(data_crispy_npv_change_plot, aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var), fill = !!rlang::sym(y_var))) +
    geom_col(width = bar_width) +
    scale_fill_gradient2(
      low = plot_color_gradient[1],
      mid = plot_color_gradient[2],
      high = plot_color_gradient[3],
      midpoint = 0,
      labels = scales::percent
    ) +
    # scale_x_discrete(position = "bottom", labels = r2dii.plot::to_title) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Crispy npv change", x = "Sector") +
    r2dii.plot::theme_2dii() +
    theme(legend.position = "none")


  return(crispy_npv_change_plot)
}

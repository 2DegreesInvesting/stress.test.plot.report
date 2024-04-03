box::use(
  dplyr[select_at],
  ggplot2[
    ggplot, aes, geom_col, geom_tile, scale_fill_gradient2, labs, theme, element_text,
    scale_x_discrete, scale_y_continuous, scale_fill_manual
  ]
)

pipeline_crispy_exposure_change_plot <- function(
    analysis_data,
    x_var = "ald_sector",
    y_exposure_var = "exposure_value_usd",
    y_value_loss_var = "crispy_value_loss",
    facet_var = NULL) {
  data_exposure_change <- prepare_for_exposure_change_plot(
    analysis_data = analysis_data,
    x_var = x_var,
    y_exposure_var = y_exposure_var,
    y_value_loss_var = y_value_loss_var
  )

  exposure_change_plot <- draw_exposure_change_plot(
    data_exposure_change = data_exposure_change,
    x_var = x_var,
    y_exposure_var = y_exposure_var,
    y_value_loss_var = y_value_loss_var,
    facet_var = facet_var
  )

  return(exposure_change_plot)
}

prepare_for_exposure_change_plot <- function(analysis_data, x_var, y_exposure_var, y_value_loss_var) {
  data_exposure_change <- analysis_data |>
    dplyr::select_at(
      c(x_var, y_exposure_var, y_value_loss_var)
    )
  return(data_exposure_change)
}


draw_exposure_change_plot <- function(
    data_exposure_change,
    x_var,
    y_exposure_var,
    y_value_loss_var,
    facet_var = NULL) {
  plot_bar_color <-
    r2dii.colours::palette_1in1000_plot |>
    dplyr::filter(.data$label == "grey") |>
    dplyr::pull(.data$hex)

  # HARDCODED PARAMETERS
  plot_color_gradient <- c(
    r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "red") |> dplyr::pull(.data$hex),
    r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "green") |> dplyr::pull(.data$hex)
  )
  bar_width <- 0.9 # Adjust as needed TODO variabiliser conf

  # PLOTTING

  exposure_change_plot <- ggplot(data_exposure_change, aes(x = !!rlang::sym(x_var))) +
    geom_col(aes(y = !!rlang::sym(y_exposure_var)), width = bar_width, fill = plot_bar_color) +
    geom_tile(
      aes(
        y = !!rlang::sym(y_exposure_var) + (!!rlang::sym(y_value_loss_var)/2),
        height = abs(!!rlang::sym(y_value_loss_var)),
        fill = dplyr::if_else(!!rlang::sym(y_value_loss_var) < 0, "Loss", "Gain")
      ),
      width = bar_width
    ) +
    scale_fill_manual(
      name = "Crispy value change",
      values = c(plot_color_gradient[1], plot_color_gradient[2]),
      breaks = c("Loss", "Gain")
    ) +
    labs(y = "Value USD", x = "") +
    r2dii.plot::theme_2dii() +
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme(
      # legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(title = "Estimated impact of the Shock on Exposure")

  if (!is.null(facet_var)) {
    exposure_change_plot <- exposure_change_plot +
      ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)), scales = "free_y", ncol = 1)
  }

  return(exposure_change_plot)
}

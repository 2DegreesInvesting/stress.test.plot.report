pipeline_crispy_pd_term_plot <- function(crispy_data_agg, facet_var = "ald_sector") {
  data_pd_term <- prepare_for_pd_term_plot(
    crispy_data_agg = crispy_data_agg,
    facet_var = facet_var
  )
  pd_term_plot <- draw_pd_term_plot(
    data_pd_term = data_pd_term,
    facet_var = facet_var
  )

  return(pd_term_plot)
}




prepare_for_pd_term_plot <- function(crispy_data_agg, facet_var) {
  data_pd_term <- crispy_data_agg |>
    tidyr::pivot_longer(
      cols = tidyr::starts_with("pd_"),
      names_to = "pd_type",
      values_to = "pd_value",
      names_prefix = "pd_"
    ) |>
    dplyr::mutate(
      pd_type = factor(.data$pd_type, levels = c("baseline", "shock", "difference"))
    ) |>
    dplyr::filter(.data$pd_type != "difference") |>
    dplyr::select_at(c(facet_var, "term", "pd_type", "pd_value"))

  return(data_pd_term)
}



draw_pd_term_plot <- function(data_pd_term, facet_var) {
  red_hex_color <- r2dii.colours::palette_1in1000_plot |>
    dplyr::filter(.data$label == "red") |>
    dplyr::pull(.data$hex)
  green_hex_color <- r2dii.colours::palette_1in1000_plot |>
    dplyr::filter(.data$label == "green") |>
    dplyr::pull(.data$hex)
  grey_hex_color <- r2dii.colours::palette_1in1000_plot |>
    dplyr::filter(.data$label == "grey") |>
    dplyr::pull(.data$hex)


  pd_term_plot <- ggplot2::ggplot(data_pd_term, ggplot2::aes(x = as.factor(term), y = pd_value, fill = pd_value)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::facet_grid(stats::as.formula(paste(paste(facet_var, collapse = "+"), "~ pd_type")), scales = "free_y") +
    ggplot2::scale_fill_gradient2(
      low = green_hex_color,
      high = red_hex_color,
      mid = grey_hex_color,
      midpoint = 0,
      limit = c(min(data_pd_term$pd_value), max(data_pd_term$pd_value)),
      space = "Lab"
    ) +
    r2dii.plot::theme_2dii() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "Term", y = "PD Value", fill = "PD Type", title = "PD Values by Term, Type, and Business Unit")

  return(pd_term_plot)
}

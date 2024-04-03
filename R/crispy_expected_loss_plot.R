pipeline_crispy_expected_loss_plot <- function(
    analysis_data,
    facet_var) {
  data_expected_loss_plot <- prepare_for_expected_loss_plot(
    analysis_data = analysis_data,
    facet_var = facet_var
  )

  expected_loss_plot <- draw_exposure_change_plot(
    data_expected_loss_plot,
    x_var = "el_type",
    y_exposure_var = "exposure_value_usd",
    y_value_loss_var = "el_value",
    facet_var = facet_var
  )
  return(expected_loss_plot)
}

prepare_for_expected_loss_plot <- function(analysis_data, facet_var) {
  data_expected_loss_plot <- analysis_data |>
    tidyr::pivot_longer(
      cols = tidyr::starts_with("expected_loss_"),
      names_to = "el_type",
      values_to = "el_value",
      names_prefix = "expected_loss_"
    ) |>
    dplyr::filter(!.data$el_type %in% c("difference", "portfolio")) |>
    dplyr::group_by_at(c(facet_var, "el_type")) |>
    dplyr::summarise(
      el_value = sum(.data$el_value, na.rm = T),
      exposure_value_usd = sum(.data$exposure_value_usd, na.rm = T)
    ) |>
    dplyr::select_at(c(facet_var, "exposure_value_usd", "el_type", "el_value"))
  return(data_expected_loss_plot)
}

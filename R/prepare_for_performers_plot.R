#' Title
#'
#' @param group_variable_char ex: "ald_sector"
#' @param value_to_plot_char value_to_plot_char
#' @param performer_variable_char ex: "company_id", "ald_business_unit"
#'
#' @export
#'
prepare_for_performers_plot <-
  function(analysis_data,
           group_variable_vec,
           performer_variable_char,
           performance_metric,
           n_performers = 10) {
    data_performers_plot <- analysis_data_single_run |>
      dplyr::group_by_at(c(group_variable_vec, performer_variable_char)) |>
      dplyr::summarise({{ performance_metric }} := sum(!!rlang::sym(performance_metric))) |>
      dplyr::slice_max(order_by = !!rlang::sym(performance_metric), n = n_performers) |>
      dplyr::ungroup()

    return(data_performers_plot)
  }

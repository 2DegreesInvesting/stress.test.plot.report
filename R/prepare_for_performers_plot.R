#' Title
#'
#' @param group_variable_char ex: "ald_sector"
#' @param value_to_plot_char ex:
#' @param performer_variable_char ex: "company_id", "ald_business_unit"
#'
#' @return
#' @export
#'
#' @examples
prepare_for_performers_plot <-
  function(analysis_data_single_run,
           group_variable_char,
           performer_variable_char,
           value_to_plot_char,
           n_performers = 10) {
    data_performers_plot <- analysis_data_single_run |>
      dplyr::rename(
        group_variable = !!rlang::sym(group_variable_char),
        performer_variable = !!rlang::sym(performer_variable_char),
        value_to_plot = !!rlang::sym(value_to_plot_char)
      ) |>
        dplyr::group_by(group_variable, performer_variable) |>
        dplyr::summarise(value_to_plot = sum(.data$value_to_plot)) |>
        dplyr::slice_max(order_by = .data$value_to_plot, n = n_performers) |>
        ungroup()
      # |>
      #   mutate(company_name = reorder_within(
      #     .data$performer_variable,
      #     .data$value_to_plot,
      #     .data$group_variable
      #   ))


      return(data_performers_plot)
  }

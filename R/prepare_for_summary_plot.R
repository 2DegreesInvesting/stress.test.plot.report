#' Title
#'
#' @param analysis_data_single_run
#' @param group_variable_char
#' @param value_to_plot_char
#'
#' @return
#' @export
#'
#' @examples
prepare_for_summary_plot <-
  function(analysis_data_single_run,
           group_variable_char,
           value_to_plot_char) {

    data_summary_plot <- analysis_data_single_run |>
      dplyr::mutate(
        group_variable = !!rlang::sym(group_variable_char),
        value_to_plot = !!rlang::sym(value_to_plot_char)
      )|>
      dplyr::group_by(.data$group_variable) |>
      dplyr::summarise(
        group_sum = sum(.data$value_to_plot, na.rm = TRUE),
        group_n = dplyr::n(),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = c("group_sum", "group_n"),
        names_to = "agg_sum_name",
        values_to = "agg_sum_value"
      ) |>
      dplyr::mutate(
        label = case_when(
          .data$agg_sum_name == "group_sum" ~ paste0("Total ", value_to_plot_char, " per ", group_variable_char),
          .data$agg_sum_name == "group_n" ~ "Number of companies per sector"
        )
      ) |>
      dplyr::group_by(agg_sum_name) |>
      dplyr::mutate(perc_of_total = .data$agg_sum_value / sum(.data$agg_sum_value)) |>
      dplyr::ungroup()
    return(data_summary_plot)
  }

#' Title
#'
#' @param analysis_data
#' @param group_variable_charvec
#' @param value_to_plot_char
#'
#' @return
#' @export
#'
#' @examples
prepare_for_pd_plot <-
  function(analysis_data,
           group_variable_charvec,
           value_to_plot_char) {
    data_pd_plot <- analysis_data |>
      dplyr::rename(
        value_to_plot = !!rlang::sym(value_to_plot_char)
      ) |>
    dplyr::group_by(!!! rlang::syms(group_variable_charvec)) |>
      dplyr::summarise(value_to_plot = stats::median(.data$value_to_plot, na.rm = T),
                       .groups = "drop")
    return(data_pd_plot)
  }

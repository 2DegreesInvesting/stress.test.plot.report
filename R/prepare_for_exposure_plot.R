#' Title
#'
#' @description
#' the data in output of this function is NOT aggregated on the group variable
#'
#'
#' @param analysis_data
#' @param group_variable_char
#' @param value_to_plot_char
#'
#' @return
#' @export
#'
#' @examples
prepare_for_exposure_plots <- function(analysis_data,
                                          group_variable_vec,
                                          agg_fn) {
  data_exposure_plot <- analysis_data |>
  dplyr::select_at(c(group_variable_vec, "exposure_at_default"))|>
  dplyr::group_by_at(group_variable_vec) |>
  dplyr::mutate(
    exposure_at_default=agg_fn(.data$exposure_at_default)
  )
  return(data_exposure_plot)
}

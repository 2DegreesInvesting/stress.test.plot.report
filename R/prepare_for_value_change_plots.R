#' Title
#'
#' @param analysis_data
#' @param value_to_plot_char "baseline", "shock", or "portfolio"
#'
#' @return
#' @export
#'
#' @examples
prepare_for_value_change_plot <- function(analysis_data_single_run, group_variable_charvec, value_to_plot_char){

  data_plt <- analysis_data_single_run|>
    dplyr::rename(
      value_to_plot = !!rlang::sym(paste0("expected_loss_", value_to_plot_char))
    ) |>
    dplyr::group_by(!!! rlang::syms(group_variable_charvec)) |>
    dplyr::summarise(
      grp_expected_loss= sum(value_to_plot),
      grp_exposure=sum(exposure_value_usd),
      .groups="drop"
      ) |>
    dplyr::mutate(
      value_change_absolute = -(grp_exposure - grp_expected_loss),
      value_change_percent = -(grp_expected_loss / grp_exposure)
      ) |>
    dplyr::select(-c(grp_expected_loss, grp_exposure))


  return(data_plt)

}

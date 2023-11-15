# prepare_for_cdi_plots <- function(analysis_data, group_variables_vec, metrics, agg_fn){
#   # Sum over sector and portfolio
#   # TODO tests
#   data_cdi_el_plot <- analysis_data %>%
#     dplyr::group_by_at(group_variables_vec) %>%
#     dplyr::summarise_at(.vars = metrics, .funs = agg_fn) %>%
#     dplyr::ungroup()
#
#   return(data_cdi_el_plot)
# }



prepare_for_cdi_npv_plots <-
  function(analysis_data,
           group_variables_vec,
           metrics_npv) {
    stopifnot(all(
      metrics_npv %in% c(
        "crispy.net_present_value_baseline",
        "crispy.net_present_value_shock",
        "net_present_value_difference"
      )
    ))


    # Sum over sector and portfolio
    # TODO tests
    data_cdi_el_plot <- analysis_data %>%
      dplyr::group_by_at(group_variables_vec) %>%
      dplyr::summarise_at(.vars = metrics_npv, .funs = sum) %>%
      dplyr::ungroup()

    return(data_cdi_el_plot)
  }


prepare_for_cdi_pd_plots <-
  function(analysis_data,
           group_variables_vec,
           weight_variable_char = NULL,
           metrics_pd) {
    stopifnot(all(
      metrics_pd %in%  c("crispy.pd_baseline", "crispy.pd_shock", "pd_difference")
    ))

    # TODO tests
    if (!is.null(weight_variable_char)) {
      aggregated_pds <- analysis_data %>%
        dplyr::group_by_at(group_variables_vec) %>%
        dplyr::summarise_at(.vars = metrics_pd,
                            .funs = stats::weighted.mean(., w = !!rlang::sym(weight_variable_char))) %>%
        dplyr::ungroup()
    } else {
      aggregated_pds <- analysis_data %>%
        dplyr::group_by_at(group_variables_vec) %>%
        dplyr::summarise_at(.vars = metrics_pd,
                            .funs = mean) %>%
        dplyr::ungroup()
    }


    return(aggregated_pds)
  }


#' Title
#'
#' @param analysis_data
#' @param group_variables_vec
#' @param metrics_el
#'
#' @return
#' @export
#'
#' @examples
prepare_for_cdi_el_plots <- function(analysis_data,
                                     group_variables_vec = group_variables_vec,
                                     metrics_el = metrics_el)    {
  stopifnot(all(
    metrics_el %in% c(
      "expected_loss_portfolio",
      "expected_loss_baseline",
      "expected_loss_shock"
    )
  ))

  # TODO tests
  aggregated_pds <- analysis_data %>%
    dplyr::group_by_at(group_variables_vec) %>%
    dplyr::summarise_at(.vars = metrics_el, .funs = sum) %>%
    dplyr::ungroup()

}

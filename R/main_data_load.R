


#' Title
#'
#' @param max_portfolio_granularity
#'
#' @return
#' @export
#'
#' @examples
main_load_portfolio_data <-
  function(raw_portfolio_data,
           maturity_month_term_bridge,
           max_portfolio_granularity,
           trisk_start_year) {

    portfolio_data <- raw_portfolio_data |>
      map_portfolio_maturity_to_term(maturity_month_term_bridge = maturity_month_term_bridge,
                                     trisk_start_year = trisk_start_year) |>
      aggregate_portfolio_facts(group_cols = max_portfolio_granularity)

    return(portfolio_data)
  }

#' Title
#'
#' @param crispy_outputs_dir
#' @param max_crispy_granularity
#'
#' @return
#' @export
#'
#' @examples
main_load_multi_crispy_data <-
  function(crispy_outputs_dir,
           max_crispy_granularity) {
    multi_crispy_data <-
      load_multiple_crispy(crispy_outputs_dir = crispy_outputs_dir) |>
      aggregate_crispy_facts(group_cols = max_crispy_granularity)
    return(multi_crispy_data)
  }

# TODO load from mlflow
#' Title
#'
#' @param crispy_outputs_dir
#'
#' @return
#' @export
#'
#' @examples
main_load_trisk_run_params <- function(crispy_outputs_dir) {
  trisk_runs_params <-
    load_multiple_crispy(crispy_outputs_dir = crispy_outputs_dir) |>
    get_trisk_params()
  return(trisk_runs_params)
}

#' Title
#'
#' @param portfolio_data
#' @param multi_crispy
#' @param portfolio_crispy_merge_cols
#'
#' @return
#' @export
#'
#' @examples
main_load_analysis_data <-
  function(portfolio_data,
           multi_crispy_data,
           trisk_run_params,
           portfolio_crispy_merge_cols) {
    analysis_data <-
      create_analysis_data(portfolio_data, multi_crispy_data, portfolio_crispy_merge_cols)

    analysis_data <-
      join_trisk_parameters(analysis_data, trisk_run_params)

    analysis_data <- compute_analysis_metrics(analysis_data)

    return(analysis_data)

  }

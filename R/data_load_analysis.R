#' Data load function to generate plots
#'
#' @description
#'  The dataframe in output of this function should always be
#'  the one used as input for the plots preprocessing functions
#'
#' @param trisk_output_dir
#' @param portfolio_data_path
#' @param granularity
#' @param maturity_month_term_bridge_fp
#'
#' @return
#' @export
#'
#' @examples
load_input_plots_data <-
  function(trisk_output_dir,
           portfolio_data_path = NULL,
           granularity = c("company_id", "company_name", "ald_sector", "ald_business_unit"),
           maturity_month_term_bridge_fp = here::here("data", "maturity_month_term_bridge.csv"),
           trisk_start_year = 2022) {
    multi_crispy_data <-
      main_load_multi_crispy_data(
        trisk_output_dir = trisk_output_dir,
        max_trisk_granularity =
          c("run_id", "term", "scenario_geography", granularity)
      )

    portfolio_data <-
      main_load_portfolio_data(
        portfolio_data_path = portfolio_data_path,
        maturity_month_term_bridge_fp = maturity_month_term_bridge_fp,
        max_portfolio_granularity = c("portfolio_id", "term", granularity),
        trisk_start_year = trisk_start_year
      )

    analysis_data <-
      main_load_analysis_data(
        portfolio_data = portfolio_data,
        multi_crispy_data = multi_crispy_data,
        portfolio_crispy_merge_cols = c("term", granularity)
      )

    return(analysis_data)
  }

#' Title
#'
#'
#'
#' @param multi_crispy
#' @param portfolio_data
#' @param portfolio_crispy_merge_cols
#'
#' @return
#'
#' @examples
main_load_analysis_data <-
  function(multi_crispy_data,
           portfolio_data,
           portfolio_crispy_merge_cols) {
    analysis_data <-
      create_analysis_data(portfolio_data, multi_crispy_data, portfolio_crispy_merge_cols)

    analysis_data <- compute_analysis_metrics(analysis_data)

    return(analysis_data)
  }



#' Title
#'
#' @param max_portfolio_granularity
#'
#' @return
#'
#' @examples
main_load_portfolio_data <-
  function(portfolio_data_path,
           maturity_month_term_bridge_fp,
           max_portfolio_granularity,
           trisk_start_year) {
    portfolio_data <- load_portfolio_data(portfolio_data_path)
    maturity_month_term_bridge <- readr::read_csv(maturity_month_term_bridge_fp)

    portfolio_data <- portfolio_data |>
      map_portfolio_maturity_to_term(
        maturity_month_term_bridge = maturity_month_term_bridge,
        trisk_start_year = trisk_start_year
      ) |>
      aggregate_portfolio_facts(group_cols = max_portfolio_granularity)

    return(portfolio_data)
  }

#' Title
#'
#' @param trisk_output_dir
#' @param max_crispy_granularity
#'
#' @return
#'
#' @examples
main_load_multi_crispy_data <-
  function(trisk_output_dir,
           max_crispy_granularity) {
    multi_crispy_data <-
      load_multiple_crispy(trisk_output_dir = trisk_output_dir) |>
      aggregate_crispy_facts(group_cols = max_crispy_granularity)
    return(multi_crispy_data)
  }

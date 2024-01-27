#' Data load function to generate plots
#'
#' @description
#'  The dataframe in output of this function should always be
#'  the one used as input for the plots preprocessing functions
#'
#' @param crispy_outputs_dir crispy_outputs_dir
#' @param portfolio_data_path portfolio_data_path
#' @param granularity granularity
#' @param maturity_month_term_bridge_fp see in data-raw in the source package
#' @param trisk_start_year (default) sets to the earliest year of multi_cripy_data
#'
#' @return
#' @export
#'
load_input_plots_data_from_files <-
  function(crispy_outputs_dir,
           portfolio_data_path = NULL,
           granularity = c("company_id", "company_name", "ald_sector", "ald_business_unit")) {
    multi_crispy_data <-
      load_multiple_crispy(crispy_outputs_dir = crispy_outputs_dir) |>
      main_load_multi_crispy_data(
        granularity = granularity
      )

    stopifnot(length(unique(multi_crispy_data$start_year)) == 1)
    trisk_start_year <- unique(multi_crispy_data$start_year)[1]

    portfolio_data <-
      load_portfolio_data(portfolio_data_path) |>
      main_load_portfolio_data(
        granularity = granularity,
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

#' Data load function to generate plots
#'
#' @description
#'  The dataframe in output of this function should always be
#'  the one used as input for the plots preprocessing functions
#'
#' @param crispy_outputs_dir crispy_outputs_dir
#' @param portfolio_data_path portfolio_data_path
#' @param granularity granularity
#' @param trisk_start_year (default) sets to the earliest year of multi_cripy_data
#'
#' @return
#' @export
#'
load_input_plots_data_from_tibble <-
  function(multi_crispy_data,
           portfolio_data = tibble::tibble(),
           granularity = c("company_id", "company_name", "ald_sector", "ald_business_unit"),
           trisk_start_year = NA) {
    multi_crispy_data <-
      multi_crispy_data |>
      main_load_multi_crispy_data(
        max_crispy_granularity =
          c("run_id", "term", "scenario_geography", granularity)
      )
    stopifnot(length(unique(multi_crispy_data$start_year)) == 1)
    trisk_start_year <- unique(multi_crispy_data$start_year)[1]

    portfolio_data <-
      portfolio_data |>
      main_load_portfolio_data(
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
#' @param multi_crispy multi_crispy
#' @param portfolio_data portfolio_data
#' @param portfolio_crispy_merge_cols portfolio_crispy_merge_cols
#'
#' @return
#'
#' @export
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
#' @param max_portfolio_granularity max_portfolio_granularity
#'
#' @return
#'
#' @export
main_load_portfolio_data <-
  function(portfolio_data,
           granularity,
           trisk_start_year) {
    portfolio_data <- portfolio_data |>
      map_portfolio_maturity_to_term(
        trisk_start_year = trisk_start_year
      ) |>
      aggregate_portfolio_facts(group_cols = granularity)

    return(portfolio_data)
  }

#' Title
#'
#' @param crispy_outputs_dir crispy_outputs_dir
#' @param max_crispy_granularity max_crispy_granularity
#'
#' @return
#'
#' @export
main_load_multi_crispy_data <-
  function(multi_crispy_data,
           granularity) {
    multi_crispy_data <- multi_crispy_data |>
      aggregate_crispy_facts(group_cols = granularity) |>
      remove_outliers_per_group(group_cols = granularity)
    return(multi_crispy_data)
  }



#' Title
#'
#' @param company_trajectories_data company_trajectories_data
#' @param max_trajectories_granularity max_trajectories_granularity
#'
#' @return
#' @export
#'
main_data_load_trajectories_data <- function(company_trajectories_data, granularity) {
  company_trajectories_data <- company_trajectories_data |>
    aggregate_trajectories_facts(group_cols = granularity) |>
    convert_trajectories_as_percentages(group_cols = granularity)
  return(company_trajectories_data)
}

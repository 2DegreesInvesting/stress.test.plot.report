#' Data load function to generate plots
#'
#' @description
#'  The dataframe in output of this function should always be
#'  the one used as input for the plots preprocessing functions
#'
#' @param crispy_outputs_dir crispy_outputs_dir
#' @param portfolio_data_path portfolio_data_path
#' @param filter_outliers filter_outliers
#' @param granularity granularity
#'
#' @export
#'
load_input_plots_data_from_files <-
  function(crispy_outputs_dir,
           portfolio_data_path = NULL,
           granularity = c("company_id", "company_name", "ald_sector", "ald_business_unit"),
           filter_outliers = FALSE) {
    multi_crispy_data <-
      load_multiple_crispy(crispy_outputs_dir = crispy_outputs_dir) |>
      main_load_multi_crispy_data(
        granularity = granularity,
        filter_outliers = filter_outliers
      )

    stopifnot(length(unique(multi_crispy_data$start_year)) == 1)
    trisk_start_year <- unique(multi_crispy_data$start_year)[1]

    portfolio_data <-
      load_portfolio_data(portfolio_data_path) |>
      main_load_portfolio_data(
        granularity = granularity,
        trisk_start_year = trisk_start_year
      )

    portfolio_crispy_merge_cols <- dplyr::intersect(colnames(multi_crispy_data), colnames(portfolio_data))
    stopifnot(length(portfolio_crispy_merge_cols) > 0)

    analysis_data <-
      main_load_analysis_data(
        portfolio_data = portfolio_data,
        multi_crispy_data = multi_crispy_data,
        portfolio_crispy_merge_cols = portfolio_crispy_merge_cols
      )

    return(analysis_data)
  }

#' Data load function to generate plots
#'
#' @description
#'  The dataframe in output of this function should always be
#'  the one used as input for the plots preprocessing functions
#'
#' @param granularity granularity
#' @param trisk_start_year (default) sets to the earliest year of multi_cripy_data
#' @param multi_crispy_data multi_crispy_data
#' @param portfolio_data portfolio_data
#' @param filter_outliers filter_outliers
#'
#' @export
#'
load_input_plots_data_from_tibble <-
  function(multi_crispy_data,
           portfolio_data = tibble::tibble(),
           granularity = c("company_id", "company_name", "ald_sector", "ald_business_unit"),
           trisk_start_year = NA, # TODO REMOVE THIS REDUNDANT PARAMETER
           filter_outliers = FALSE) {
    multi_crispy_data <-
      multi_crispy_data |>
      main_load_multi_crispy_data(
        granularity = granularity,
        filter_outliers = filter_outliers
      )

    stopifnot(length(unique(multi_crispy_data$start_year)) <= 1)
    trisk_start_year <- unique(multi_crispy_data$start_year)[1]

    portfolio_data <-
      portfolio_data |>
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


#' main load analysis data
#'
#'
#'
#' @param portfolio_data portfolio_data
#' @param portfolio_crispy_merge_cols portfolio_crispy_merge_cols
#' @param multi_crispy_data multi_crispy_data
#'
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
#' @param portfolio_data portfolio_data
#' @param granularity granularity
#' @param param_cols param_cols
#' @param trisk_start_year trisk_start_year
#'
#'
#' @export
main_load_portfolio_data <-
  function(portfolio_data,
           granularity,
           param_cols = c("portfolio_id", "term", "asset_type"),
           trisk_start_year) {
    group_cols <- unique(c(granularity, param_cols))

    portfolio_data <- portfolio_data |>
      map_portfolio_maturity_to_term(
        trisk_start_year = trisk_start_year
      )
    #  aggregate_portfolio_facts(group_cols = group_cols)

    return(portfolio_data)
  }

#' Title
#'
#' @param multi_crispy_data multi_crispy_data
#' @param granularity granularity
#' @param param_cols param_cols
#' @param filter_outliers filter_outliers
#'
#'
#' @export
main_load_multi_crispy_data <-
  function(multi_crispy_data,
           granularity, param_cols = c(
             "term", "run_id", "roll_up_type", "scenario_geography", "baseline_scenario",
             "shock_scenario", "risk_free_rate", "discount_rate", "div_netprofit_prop_coef",
             "carbon_price_model", "market_passthrough", "financial_stimulus", "start_year",
             "growth_rate", "shock_year"
           ),
           filter_outliers = FALSE) {
    group_cols <- unique(c(granularity, param_cols))

    multi_crispy_data <- multi_crispy_data |>
      aggregate_crispy_facts(group_cols = group_cols)

    # Conditionally apply remove_outliers_per_group based on the filter_outliers parameter
    multi_crispy_data <- if (filter_outliers) {
      multi_crispy_data |>
        remove_outliers_per_group(
          group_cols = group_cols,
          column_filtered = "crispy_perc_value_change"
        )
    } else {
      multi_crispy_data
    }
    return(multi_crispy_data)
  }



#' Title
#'
#' @param company_trajectories_data company_trajectories_data
#' @param max_trajectories_granularity max_trajectories_granularity
#'
#' @export
#'
main_data_load_trajectories_data <- function(company_trajectories_data, granularity, param_cols = c(
                                               "run_id", "year"
                                             )) {
  group_cols <- unique(c(granularity, param_cols))

  company_trajectories_data <- company_trajectories_data |>
    aggregate_trajectories_facts(group_cols = group_cols)
  # convert_trajectories_as_percentages(group_cols = group_cols[group_cols != "year"])
  return(company_trajectories_data)
}

#' Title
#'
#' @param crispy_outputs_dir crispy_outputs_dir
#' @param granularity granularity
#' @param param_cols param_cols
#'
#' @export
#'
main_data_load_trajectories_data_from_file <- function(
    crispy_outputs_dir,
    granularity, param_cols = c("run_id", "year")) {
  company_trajectories_data <- load_multiple_trajectories(crispy_outputs_dir) |>
    main_data_load_trajectories_data(granularity = granularity, param_cols = param_cols)
  return(company_trajectories_data)
}

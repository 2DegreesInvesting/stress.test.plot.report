
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
    ...) {
  company_trajectories_data <- load_multiple_trajectories(crispy_outputs_dir) |>
    main_data_load_trajectories_data(...)
  return(company_trajectories_data)
}


#' Title
#'
#' @param company_trajectories_data company_trajectories_data
#' @param max_trajectories_granularity max_trajectories_granularity
#'
#' @export
#'
main_data_load_trajectories_data <- function(
    company_trajectories_data,
    granularity=c("company_id", "company_name", "ald_sector", "ald_business_unit"),
    param_cols = c(
      "run_id", "year","scenario_geography_arg", "baseline_scenario_arg",
      "shock_scenario_arg", "risk_free_rate_arg", "discount_rate_arg", "div_netprofit_prop_coef_arg",
      "carbon_price_model_arg", "market_passthrough_arg", "financial_stimulus_arg",
      "growth_rate_arg", "shock_year_arg")
      ) {
  group_cols <- unique(c(granularity, param_cols))

  company_trajectories_data <- company_trajectories_data |>
    aggregate_trajectories_facts(group_cols = group_cols)  
  return(company_trajectories_data)
}


load_multiple_trajectories <- function(crispy_outputs_dir) {
  # Required Libraries

  # Get file paths
  files_path <- list.files(
    path = crispy_outputs_dir,
    pattern = "^company_trajectories_(.*).csv",
    recursive = TRUE,
    full.names = TRUE
  )

  stopifnot(length(files_path) > 0)

  # Load all files into a list and add a run_id column for each dataframe
  data_list <- purrr::map(files_path, function(fp) {
    df <- readr::read_csv(fp,
      col_types = readr::cols_only(
        run_id = "c",
        company_id = "c",
        company_name = "c",
        ald_sector = "c",
        ald_business_unit = "c",
        year = "d",
        phase_out = "d",
        baseline_scenario_arg = "c",
        shock_scenario_arg = "c",
        scenario_geography_arg = "c",
        risk_free_rate_arg = "d",
        discount_rate_arg = "d",
        div_netprofit_prop_coef_arg = "d",
        growth_rate_arg = "d",
        shock_year_arg = "d",
        carbon_price_model_arg = "c",
        market_passthrough_arg = "d",
        financial_stimulus_arg = "d",
        production_plan_company_technology = "d",
        production_baseline_scenario = "d",
        production_target_scenario = "d",
        production_shock_scenario = "d",
        price_baseline_scenario = "d",
        price_shock_scenario = "d",
        net_profits_baseline_scenario = "d",
        net_profits_shock_scenario = "d",
        discounted_net_profits_baseline_scenario = "d",
        discounted_net_profits_shock_scenario = "d"
      )
    ) |>
    dplyr::rename(
      baseline_scenario = baseline_scenario_arg,
      shock_scenario = shock_scenario_arg,
      scenario_geography = scenario_geography_arg,
      risk_free_rate=risk_free_rate_arg,
      discount_rate=discount_rate_arg,
      div_netprofit_prop_coef=div_netprofit_prop_coef_arg,
      growth_rate=growth_rate_arg,
      shock_year=shock_year_arg,
      carbon_price_model=carbon_price_model_arg,
      market_passthrough=market_passthrough_arg,
      financial_stimulus=financial_stimulus_arg
    ) |>
    dplyr::filter(.data$year < max(.data$year)) # removes last year that is NA
  })

  multi_trajectories_data <- dplyr::bind_rows(data_list)

  return(multi_trajectories_data)
}

#' Aggregate numerical trajectories columns
#'
#' @param multi_trajectories dataframe of trajectories from 1 or multiple trisk truns
#' @param group_cols group_cols
#'
#' @export
#'
aggregate_trajectories_facts <- function(multi_trajectories, group_cols) {
    multi_trajectories <- multi_trajectories |>
      dplyr::group_by_at(group_cols) |>
      dplyr::summarise(
        production_baseline_scenario = sum(.data$production_baseline_scenario, na.rm = TRUE),
        production_target_scenario = sum(.data$production_target_scenario, na.rm = TRUE),
        production_shock_scenario = sum(.data$production_shock_scenario, na.rm = TRUE),
        price_baseline_scenario= mean(.data$price_baseline_scenario, na.rm = TRUE),
        price_shock_scenario = mean(.data$price_shock_scenario, na.rm = TRUE),
        net_profits_baseline_scenario = sum(.data$net_profits_baseline_scenario, na.rm = TRUE),
        net_profits_shock_scenario = sum(.data$net_profits_shock_scenario, na.rm = TRUE),
        discounted_net_profits_baseline_scenario = sum(.data$discounted_net_profits_baseline_scenario, na.rm = TRUE),
        discounted_net_profits_shock_scenario = sum(.data$discounted_net_profits_shock_scenario, na.rm = TRUE),
        .groups = "drop"
      )
    return(multi_trajectories)
  }


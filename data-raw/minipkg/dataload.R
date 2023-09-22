load_equity_exposures <- function(equity_input_path){

  equity_exposures <- readr::read_csv(
    equity_input_path,
    col_types = readr::cols_only(
      # holding_id = "c",
      insurance_category = "c",
      value_usd = "d",
      asset_type = "c",
      ar_company_id = "d"
    )
  )

  #quick equity manipulation
  equity_exposures <-
    equity_exposures %>%
    dplyr::filter(asset_type == "Equity") %>%
    dplyr::select(-c("asset_type"))

  ## add cpr prefix pre-matching
  names(equity_exposures) <-
    paste0("cpr_", names(equity_exposures))

  equity_exposures <- equity_exposures %>%
    dplyr::rename(company_id = cpr_ar_company_id)

  return(equity_exposures)
}

load_bond_data <- function(bonds_input_path){

  bond_data <- readr::read_csv(
    bonds_input_path,
    col_types = readr::cols_only(
      isin = "c",
      insurance_category = "c",
      asset_type = "c",
      ar_company_id = "d",
      value_usd = "d",
      first_maturity = "c",
      term = "d"
    )
  ) %>% rename(company_id = ar_company_id)

  return(bond_data)
}


load_multi_crispy_outputs <- function(trisk_output_path) {
  dataset_files <-
    list.files(
      path = trisk_output_path,
      pattern = "crispy_",
      recursive = TRUE,
      full.names = TRUE
    )

  data_list <- list()

  all_crispy <- NULL
  for (file in dataset_files) {
    crispy <-  readr::read_csv(
      file,
      col_types = readr::cols_only(
        company_id = "d",
        company_name = "c",
        sector = "c",
        business_unit = "c",
        roll_up_type = "c",
        scenario_geography = "c",
        baseline_scenario = "c",
        shock_scenario = "c",
        # lgd = "d",
        risk_free_rate = "d",
        discount_rate = "d",
        dividend_rate = "d",
        growth_rate = "d",
        shock_year = "d",
        net_present_value_baseline = "d",
        net_present_value_shock = "d",
        net_present_value_difference = "d",
        term = "d",
        pd_baseline = "d",
        pd_shock = "d",
        pd_difference = "d",
      )
    )
    all_crispy <- dplyr::bind_rows(all_crispy, crispy) %>%
      dplyr::mutate(scenario_duo = paste0(baseline_scenario, "__", shock_scenario))
  }
  return(all_crispy)
}

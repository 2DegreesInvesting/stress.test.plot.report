load_loans_data <- function(loans_input_path){

  loans_data <- readr::read_csv(
    loans_input_path,
    col_types = readr::cols_only(
      bank="c",
      company_name = "c",
      sector = "c",
      exposure_value = "d",
      term = "i",
      lgd = "d"
    )
  )
  return(loans_data)
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
        company_name = "c",
        sector = "c",
        # business_unit = "c",
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
        # net_present_value_baseline = "d",
        # net_present_value_shock = "d",
        # net_present_value_difference = "d",
        term = "d",
        pd_baseline = "d",
        pd_shock = "d",
        pd_difference = "d",
      )
    )

    crispy <- crispy %>%
      dplyr::group_by(company_name, sector, roll_up_type, scenario_geography,
                      baseline_scenario, shock_scenario, risk_free_rate,
                      dividend_rate, growth_rate, shock_year,
                      term) %>%

      dplyr::summarise(pd_baseline=mean(pd_baseline),
                       pd_shock=mean(pd_shock),
                       pd_difference=mean(pd_difference),
                       .groups="drop")

    crispy <- crispy %>% dplyr::mutate(
      run_id = stringr::str_replace(
        stringr::str_replace(basename(file), "crispy_output_", ""),
        ".csv",
        ""
      ),
      scenario_duo = paste0(baseline_scenario, "__", shock_scenario)
    )
    all_crispy <- dplyr::bind_rows(all_crispy, crispy)
  }
  return(all_crispy)
}

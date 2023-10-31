load_portfolio_data <- function(portfolio_data_path){
  portfolio_data <- readr::read_csv(
    portfolio_data_path,
    col_types = readr::cols_only(
      portfolio_id="c",
      company_id="i",
      asset_type="c",
      ald_sector="c",
      ald_business_unit="c",
      ald_location="c",
      exposure_value_usd="d",
      expiration_date=readr::col_date(format="%Y-%m-%d"),
      loss_given_default="d"
    )
  )
  return(portfolio_data)
}


load_multiple_crispy <- function(crispy_outputs_dir) {
  # Required Libraries

  # Get file paths
  files_path <- list.files(
    path = crispy_outputs_dir,
    pattern = "^crispy_output_*",
    recursive = TRUE,
    full.names = TRUE
  )

  # Load all files into a list and add a UUID column for each dataframe
  data_list <- purrr::map(files_path, function(file_path) {
    df <- arrow::read_parquet(file_path) |>
      dplyr::mutate(
        run_id=stringr::str_remove(basename(file_path), ".parquet")
        )
  })
  multi_crispy <- dplyr::bind_rows(data_list)
  return(multi_crispy)
}

map_portfolio_maturity_to_term <- function(portfolio_data, maturity_month_term_bridge, start_year=2022) {
  
  
  min_maturity_month <- min(maturity_month_term_bridge$portfolio_maturity_month)
  max_maturity_month <- max(maturity_month_term_bridge$portfolio_maturity_month)

  start_date <- as.Date(paste0(start_year,"-01-01"))
  
  portfolio_data <- portfolio_data |> 
    # dplyr::mutate(portfolio_maturity_month=expiration_date-start_date) |> 
    dplyr::mutate(portfolio_maturity_month=lubridate::interval(.env$start_date, .data$expiration_date) %/% months(1)) |> 
    dplyr::mutate(portfolio_maturity_month=dplyr::if_else(
      .data$portfolio_maturity_month < .env$min_maturity_month, 
      .env$min_maturity_month, 
      .data$portfolio_maturity_month)) |>
    dplyr::mutate(portfolio_maturity_month=dplyr::if_else(
      .data$portfolio_maturity_month > .env$max_maturity_month, 
      .env$max_maturity_month, 
      .data$portfolio_maturity_month)) |>
    dplyr::select(-c(.data$expiration_date))

  portfolio_data <- portfolio_data |> 
    dplyr::left_join(maturity_month_term_bridge, by="portfolio_maturity_month") |>
    dplyr::select(-c(.data$portfolio_maturity_month))

  return(portfolio_data)
  
}


create_analysis_data <- function(portfolio_data, multi_crispy){
  analysis_data <- dplyr::left_join(portfolio_data, multi_crispy, by=c("company_id", "ald_sector", "ald_business_unit", "term"))
  return(analysis_data)
}

# load_multi_crispy_outputs <- function(trisk_output_path) {
#   dataset_files <-
#     list.files(
#       path = trisk_output_path,
#       pattern = "crispy_",
#       recursive = TRUE,
#       full.names = TRUE
#     )

#   data_list <- list()

#   all_crispy <- NULL
#   for (file in dataset_files) {
#     crispy <-  readr::read_csv(
#       file,
#       col_types = readr::cols_only(
#         company_name = "c",
#         sector = "c",
#         # business_unit = "c",
#         roll_up_type = "c",
#         scenario_geography = "c",
#         baseline_scenario = "c",
#         shock_scenario = "c",
#         # lgd = "d",
#         risk_free_rate = "d",
#         discount_rate = "d",
#         dividend_rate = "d",
#         growth_rate = "d",
#         shock_year = "d",
#         # net_present_value_baseline = "d",
#         # net_present_value_shock = "d",
#         # net_present_value_difference = "d",
#         term = "d",
#         pd_baseline = "d",
#         pd_shock = "d",
#         pd_difference = "d",
#       )
#     )

#     crispy <- crispy %>%
#       dplyr::group_by(company_name, sector, roll_up_type, scenario_geography,
#                       baseline_scenario, shock_scenario, risk_free_rate,
#                       dividend_rate, growth_rate, shock_year,
#                       term) %>%
#       dplyr::summarise(pd_baseline=mean(pd_baseline),
#                        pd_shock=mean(pd_shock),
#                        pd_difference=mean(pd_difference),
#                        .groups="drop")

#     crispy <- crispy %>% dplyr::mutate(
#       run_id = stringr::str_replace(
#         stringr::str_replace(basename(file), "crispy_output_", ""),
#         ".csv",
#         ""
#       ),
#       scenario_duo = paste0(baseline_scenario, "__", shock_scenario)
#     )
#     all_crispy <- dplyr::bind_rows(all_crispy, crispy)
#   }
#   return(all_crispy)
# }

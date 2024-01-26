box::use(
  `data-raw` / scripts / constant[mock_filepaths]
)


company_activities <- readr::read_csv(mock_filepaths$company_activities)
company_emissions <- readr::read_csv(mock_filepaths$company_emissions)

# ===================================
# ABCD STRESS TEST INPUT
# ===================================

start_year <- 2020
time_horizon <- 10
additional_year <- NULL
sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")
km_per_vehicle <- 15000

abcd_stress_test_input <-
  STDataMGMT::prepare_abcd_data(
    company_activities = company_activities,
    company_emissions = company_emissions,
    scenarios_geographies = STDataMGMT::scenarios_geographies,
    start_year = start_year,
    time_horizon = time_horizon,
    additional_year = additional_year,
    km_per_vehicle = km_per_vehicle,
    sector_list = sector_list
  )

readr::write_csv(abcd_stress_test_input, mock_filepaths$abcd_stress_test_input)

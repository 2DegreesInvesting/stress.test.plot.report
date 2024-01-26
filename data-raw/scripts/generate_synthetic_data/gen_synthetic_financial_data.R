box::use(
  `data-raw` / scripts / constant[mock_filepaths]
)

company_activities <- readr::read_csv(mock_filepaths$company_activities)
eikon_data <- readr::read_csv(mock_filepaths$eikon_data)


companies_data <- company_activities |> dplyr::distinct(company_id, ald_sector, ald_location)

prewrangled_financial_data_stress_test <- STDataMGMT::prepare_financial_data(
  financial_data = eikon_data,
  companies_data = companies_data,
  ownership_tree = NULL,
  minimum_sample_size = 1,
  minimum_ratio_sample = 0,
  allowed_range_npm = c(-Inf, Inf)
)

abcd_stress_test_input <- readr::read_csv(mock_filepaths$abcd_stress_test_input)

prewrangled_financial_data_stress_test <- prewrangled_financial_data_stress_test %>%
  dplyr::inner_join(abcd_stress_test_input %>% dplyr::distinct(company_id))



readr::write_csv(prewrangled_financial_data_stress_test, mock_filepaths$prewrangled_financial_data_stress_test)

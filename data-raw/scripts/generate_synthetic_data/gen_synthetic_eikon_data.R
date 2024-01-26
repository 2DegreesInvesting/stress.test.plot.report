box::use(
  `data-raw` / scripts / constant[mock_filepaths],
  dplyr[...],
  tibble[...]
)


# TODO with those columns
# company_id, company_name, corporate_bond_ticker, pd, net_profit_margin, debt_equity_ratio, volatility

abcd_stress_test_input <- readr::read_csv(mock_filepaths$abcd_stress_test_input)

available_companies <- abcd_stress_test_input |>
  dplyr::distinct(company_id) |>
  dplyr::pull()

# random sample of companies
sample_companies <- sample(available_companies, size = length(available_companies) * 10, replace = TRUE)
# remove 50% companies at random
sample_companies <- sample_companies[!(sample_companies %in%
  sample(available_companies, size = as.integer(length(available_companies) / 2), replace = FALSE)
)]
sample_companies <- tibble(company_id = sample_companies)


random_isin <- function(...) {
  countries <- countrycode::codelist |>
    filter(!is.na(ecb)) |>
    distinct(ecb) |>
    pull(ecb)

  sprintf(
    "%s%s",
    paste0(sample(countries, 1, TRUE), collapse = ""),
    paste0(sample(9, 10, TRUE), collapse = "")
  )
}

random_isins_with_companies <- purrr::map_vec(1:nrow(sample_companies), random_isin)
random_isins_no_companies <- purrr::map_vec(1:100, random_isin)
random_isins <- tibble(isin = c(random_isins_with_companies, random_isins_no_companies))
stopifnot(nrow(random_isins) == length(unique(random_isins$isin))) # check all isin unique


eikon_data <- bind_cols(bind_rows(sample_companies, tibble(company_id = rep(NA, 100))), random_isins)

eikon_nrow <- nrow(eikon_data)

eikon_data <- eikon_data |>
  tibble::add_column(
    # corporate_bond_ticker = NA,
    pd = runif(eikon_nrow, min = 0, max = 1),
    net_profit_margin = runif(eikon_nrow, min = 0, max = 1),
    debt_equity_ratio = runif(eikon_nrow, min = 0, max = 1),
    volatility = runif(eikon_nrow, min = 0, max = 1),
    asset_drift = runif(eikon_nrow, min = 0, max = 1)
  ) |>
  mutate(ald_location = substr(isin, start = 1, stop = 2))

readr::write_csv(eikon_data, mock_filepaths$eikon_data)

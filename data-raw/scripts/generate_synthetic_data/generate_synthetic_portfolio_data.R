box::use(
  `data-raw` / scripts / constant[mock_filepaths],
  dplyr[...]
)


abcd_stress_test_input <- readr::read_csv(mock_filepaths$abcd_stress_test_input)
eikon_data <- readr::read_csv(mock_filepaths$eikon_data)

portfolio_values <- abcd_stress_test_input |>
  distinct(company_id, company_name, ald_sector, ald_business_unit) |>
  mutate(
    exposure_value_usd = sample(1e6:1e8, n(), replace = TRUE),
    expiration_date = sample(seq(as.Date("2022/02/01"), as.Date("2030/01/01"), by = "month"), n(), replace = TRUE)
  )

companies_fixed_income <-
  portfolio_values |>
  sample_n(200) |>
  left_join(eikon_data |> select(company_id, isin) |> sample_frac(0.3),
    by = "company_id",
    relationship = "many-to-many"
  )

companies_equities <-
  portfolio_values |>
  sample_n(200) |>
  mutate(
    expiration_date = NA
  )

# companies_id <-
#   all_crispy |>
#   distinct(company_id, sector, business_unit) |>
#   sample_n(200) |>
#   rename(
#     ald_sector = sector,
#     ald_business_unit = business_unit
#   )

# companies_fixed_income <-
#   bind_cols(companies[sample(1:nrow(companies)), ], bonds)
# companies_equities <-
#   bind_cols(companies[sample(1:nrow(companies)), ], equities)

portfolio_data <- bind_rows(companies_fixed_income, companies_equities)
# portfolio_data <- portfolio_data |>
#   mutate(
#     first_maturity = as.Date(portfolio_data$first_maturity, "%d.%m.%y")
#   )
portfolio_data[, "loss_given_default"] <- runif(n = nrow(portfolio_data), min = 1e-12, max = .9999999999)


countries <- countrycode::codelist |>
  filter(!is.na(ecb)) |>
  distinct(ecb) |>
  pull(ecb)
portfolio_data <- portfolio_data |>
  mutate(ald_location = if_else(!is.na(isin), substring(isin, 1, 2), sample(countries, 1, TRUE)))

# portfolio_data[rbinom(nrow(portfolio_data), 1, 0.3) == 1, "isin"] <- NA
portfolio_data[1:ceiling(nrow(portfolio_data) / 2), "portfolio_id"] <- "portfolio_A"
portfolio_data[floor(nrow(portfolio_data) / 2):nrow(portfolio_data), "portfolio_id"] <- "portfolio_B"

portfolio_data$pd_portfolio <- NA



portfolio_data <- portfolio_data |> select(
  portfolio_id,
  company_name,
  company_id,
  ald_sector,
  ald_business_unit,
  ald_location,
  exposure_value_usd,
  expiration_date,
  loss_given_default,
  pd_portfolio
)

readr::write_csv(portfolio_data, mock_filepaths$portfolio_data)

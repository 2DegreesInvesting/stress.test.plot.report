devtools::load_all()



empty_portfolio_data <- tibble::tibble(
  portfolio_id = character(),
  asset_type = character(),
  ald_sector = character(),
  ald_business_unit = character(),
  term = double(),
  exposure_value_usd = double(),
  loss_given_default = double(),
  pd_portfolio = double()
)



empty_crispy_data <- tibble::tibble(
  run_id = character(),
  scenario_geography = character(),
  ald_sector = character(),
  ald_business_unit = character(),
  term = integer(),
  roll_up_type = character(),
  baseline_scenario = character(),
  shock_scenario = character(),
  lgd = double(),
  risk_free_rate = double(),
  discount_rate = double(),
  dividend_rate = double(),
  growth_rate = double(),
  shock_year = double(),
  net_present_value_baseline = double(),
  net_present_value_shock = double(),
  pd_baseline = double(),
  pd_shock = double()
)



expected_structure_analysis_data <- tibble::tibble(
  portfolio.portfolio_id = character(),
  portfolio.asset_type = character(),
  portfolio.ald_sector = character(),
  portfolio.ald_business_unit = character(),
  portfolio.term = double(),
  portfolio.exposure_value_usd = double(),
  portfolio.loss_given_default = double(),
  portfolio.pd_portfolio = double(),
  crispy.run_id = character(),
  crispy.scenario_geography = character(),
  crispy.roll_up_type = character(),
  crispy.baseline_scenario = character(),
  crispy.shock_scenario = character(),
  crispy.lgd = double(),
  crispy.risk_free_rate = double(),
  crispy.discount_rate = double(),
  crispy.dividend_rate = double(),
  crispy.growth_rate = double(),
  crispy.shock_year = double(),
  crispy.net_present_value_baseline = double(),
  crispy.net_present_value_shock = double(),
  crispy.pd_baseline = double(),
  crispy.pd_shock = double(),
  net_present_value_difference = double(),
  net_present_value_perc_change = double(),
  exposure_at_default = double(),
  expected_loss_portfolio = double(),
  expected_loss_baseline = double(),
  expected_loss_shock = double(),
  pd_difference = double()
)

test_that("analysis load from empty dataframes works", {

  empty_analysis_data <- main_load_analysis_data(
    empty_portfolio_data,
    empty_crispy_data,
    portfolio_crispy_merge_cols =
      c("ald_sector", "ald_business_unit", "term")
  )

  # Check if column names are the same
  expect_equal(names(empty_analysis_data), names(expected_structure_analysis_data))

  # Check if data types of each column are the same
  for (col in names(expected_structure_analysis_data)) {
    expect_equal(class(empty_analysis_data[[col]]), class(expected_structure_analysis_data[[col]]))
  }
})

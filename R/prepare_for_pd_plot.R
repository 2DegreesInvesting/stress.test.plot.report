prepare_for_pd_plot <- function(data) {
  out <- data %>%
  filter(
    scenario_geography == "Global",
    investor_name == "Meta Investor",
    portfolio_name == "Meta Investor"
  ) %>%
  select(ald_sector, term, shock_year_arg, pd_change_sector_shock) %>%
  distinct()
  out
}

prepare_for_pd_plot <- function(data) {
  out <- data %>%
  filter(
    investor_name == "Meta Investor",
    portfolio_name == "Meta Investor"
  ) %>%
  select(ald_sector, term, shock_year_arg, pd_change_sector_shock) %>%
  mutate(pd_change_sector_shock = pd_change_sector_shock * 100) %>%
  distinct()
  out
}

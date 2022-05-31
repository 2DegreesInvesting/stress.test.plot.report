prepare_for_el_plots <- function(data) {
  data_summary <- prepare_for_summary_plot(data)
  data_exp_per_sector <- data_summary %>%
  filter(
    name == "group_sum"
  ) %>%
  ungroup() %>%
  select(-name) %>%
  mutate(
    ald_sector = group_variable,
    exposure = value*10^6
    ) %>%
  select(ald_sector, exposure)

  out <- data %>%
    filter(
      scenario_geography == "Global",
      investor_name == "Meta Investor",
      portfolio_name == "Meta Investor"
    ) %>%
    group_by(ald_sector) %>%
    summarise(
      expected_loss_baseline = -sum(expected_loss_baseline, na.rm = TRUE),
      expected_loss_shock = -sum(expected_loss_shock, na.rm = TRUE),
      value_change = expected_loss_shock - expected_loss_baseline
    ) %>%
    pivot_longer(
      cols = c(expected_loss_baseline, expected_loss_shock, value_change)
    ) %>%
    inner_join(data_exp_per_sector, by = "ald_sector") %>%
    mutate(
      el_as_perc_exposure = value / exposure
    )
  out
}

prepare_for_pd_plot <- function(data) {
  out <- data %>%
  select(ald_sector, term, shock_year_arg, pd_change_sector_shock) %>%
  distinct()
  out
}

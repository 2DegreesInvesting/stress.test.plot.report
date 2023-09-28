compute_scenario_sector_weights <- function(all_crispy) {
  # TODO PQ filter term and not group by term ?

  total_npv_weights <- all_crispy %>%
    dplyr::filter(term == 1) %>%
    group_by(scenario_duo, company_id) %>%
    summarize(sum_NPV_total = sum(net_present_value_baseline),
              .groups = "drop")

  sector_npv_weights <- all_crispy %>%
    dplyr::filter(term == 1) %>%
    group_by(scenario_duo, sector, company_id) %>%
    summarize(sum_NPV_sector = sum(net_present_value_baseline),
              .groups = "drop")

  scenario_sector_weights <-
    dplyr::inner_join(total_npv_weights,
                      sector_npv_weights,
                      by = c("scenario_duo", "company_id")) %>%
    mutate(sector_weight = sum_NPV_sector / sum_NPV_total)

  return(scenario_sector_weights)
}

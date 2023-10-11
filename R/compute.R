compute_portfolio_weights <- function(loans_data){
  sector_tot_exposure <- loans_data %>%
    dplyr::group_by(bank, sector) %>%
    dplyr::summarise(sum_sector_exposure_value=sum(exposure_value), .groups = "drop")
  company_sector_tot_exposure <- loans_data %>%
    dplyr::group_by(bank, sector, company_name) %>%
    dplyr::summarise(sum_sector_company_exposure_value=sum(exposure_value), .groups = "drop")

  company_sector_weights <- dplyr::inner_join(company_sector_tot_exposure, sector_tot_exposure,
                                              by = join_by(bank, sector)) %>%
    dplyr::mutate(weight=sum_sector_company_exposure_value/sum_sector_exposure_value) %>%
    dplyr::select(bank, sector, company_name, weight)
}

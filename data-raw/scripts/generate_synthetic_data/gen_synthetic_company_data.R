box::use(
  `data-raw` / scripts / constant[production_types, mock_filepaths],
  dplyr[...]
)
#' Generation of synthetic data


generate_company_name_ids <- function(
    n_companies) {
  company_name <- paste0("company", sep = "-", 1:n_companies)
  company_id <- 1:n_companies
  company_name_ids <-
    tibble::tibble(company_id = company_id, company_name = company_name)
  return(company_name_ids)
}

generate_company_sectors <- function(
    company_name_ids,
    n_multi_sector) {
  n_company <- nrow(company_name_ids)

  company_sectors <-
    company_name_ids |>
    dplyr::cross_join(production_types |> select(-emissions_factor_unit)) |>
    dplyr::group_by(company_id, company_name) |>
    dplyr::sample_n(size = n_multi_sector, replace = FALSE)

  MW_to_duplicate <-
    company_sectors |>
    dplyr::filter(ald_production_unit == "MW") |>
    dplyr::mutate(ald_production_unit = "MWh")
  MWh_to_duplicate <-
    company_sectors |>
    dplyr::filter(ald_production_unit == "MWh") |>
    dplyr::mutate(ald_production_unit = "MW")
  company_sectors <- dplyr::bind_rows(
    company_sectors,
    MW_to_duplicate,
    MWh_to_duplicate
  ) |>
    dplyr::distinct_all() # remove alternated existing prod if any
  return(company_sectors)
}

generate_company_location <- function(
    company_sectors, max_assigned_countries) {
  countries <- countrycode::codelist$ecb
  countries <- countries[!is.na(countries)]
  # subsample the country codes, so we're sure some companies will
  # have different technologies in same countries
  countries <-
    sample(countries, size = max_assigned_countries + 1, replace = FALSE)

  n_country_foreach_row <-
    sample(
      x = 1:max_assigned_countries,
      size = nrow(company_sectors),
      replace = TRUE
    )
  countries_foreach_row <-
    purrr::map(
      n_country_foreach_row,
      ~ sample(countries, size = .x, replace = FALSE)
    )
  ald_location <-
    tibble::tibble(ald_location = countries_foreach_row)
  company_location <-
    dplyr::bind_cols(company_sectors, ald_location) |>
    tidyr::unnest_longer(col = "ald_location")
  return(company_location)
}


generate_company_production <- function(
    company_location,
    n_year_plan,
    prop_na,
    nrow_full_na,
    mean_production) {
  productions_values <-
    replicate(nrow(company_location),
      rgeom(n_year_plan, 1 / mean_production),
      simplify = FALSE
    )
  productions_values <-
    tidyr::unnest_wider(
      tibble::tibble(ald_production = productions_values),
      col = "ald_production",
      names_sep = "_"
    )
  colnames(productions_values) <-
    paste0("Equity Ownership", sep = " ", 2022:(2022 + n_year_plan))

  # add random NA. Total NA match proportion parameter
  productions_values <-
    apply(productions_values, 2, function(x) {
      x[sample(c(1:nrow(productions_values)), floor(nrow(productions_values) / (1 /
        prop_na)))] <-
        NA
      x
    })

  # Add some random rows with all production values NA
  productions_values[sample(1:nrow(productions_values), nrow_full_na), ] <-
    NA

  company_production <-
    dplyr::bind_cols(company_location, productions_values)
  return(company_production)
}


#' GENERATE COMPANY ACTIVITY
#' Generative operations are repeated for companies assigned to a single sector
#' and companies assigned to multiple sectors
#'
#' Generate random unique company_id identifier
#' Generate random company name for each company_id
#' Assign random sectors (single or multiple) to each company. Force companies
#'   being assigned a production in MW to have a production in MWh, and vice-versa.
#' Assign random iso2c country codes to each company sector
#' Assign random production values at each row, add random nans, set some random rows to full nans
generate_company_activities <- function(
    n_companies = 200,
    n_multi_sector = 3,
    max_assigned_countries = 5,
    n_year_plan = 5,
    prop_na = 0.3,
    nrow_full_na = 10,
    mean_production = 1e10) {
  company_activities_single_sector <-
    generate_company_name_ids(n_companies / 2) |>
    generate_company_sectors(n_multi_sector = 1) |>
    generate_company_location(max_assigned_countries) |>
    generate_company_production(n_year_plan, prop_na, nrow_full_na, mean_production)

  company_activities_multi_sector <-
    generate_company_name_ids(n_companies / 2) |>
    generate_company_sectors(n_multi_sector = n_multi_sector) |>
    generate_company_location(max_assigned_countries) |>
    generate_company_production(n_year_plan, prop_na, nrow_full_na, mean_production)

  company_activities <-
    dplyr::bind_rows(
      company_activities_single_sector,
      company_activities_multi_sector
    )

  company_activities <- company_activities |>
    dplyr::ungroup()

  return(company_activities)
}


assign_activities_to_their_emission_unit <- function(base_data) {
  company_emission_unit <- base_data |>
    dplyr::left_join(production_types,
      by = dplyr::join_by(ald_sector, ald_business_unit, ald_production_unit)
    ) |>
    dplyr::select(-ald_production_unit)
  # dplyr::rename(ald_production_unit = emissions_factor_unit) # rename emissions_factor_unit to activity_unit bc expected format for prep abcd script
  return(company_emission_unit)
}


#' GENERATE COMPANY EMISSIONS
generate_company_emissions <- function(company_activities) {
  base_data <-
    company_activities |> dplyr::select(
      company_id,
      company_name,
      ald_sector,
      ald_business_unit,
      ald_location,
      ald_production_unit
    )
  company_emission_unit <-
    assign_activities_to_their_emission_unit(base_data)
  company_emissions <-
    generate_company_production(
      company_emission_unit,
      n_year_plan = 5,
      prop_na = 0.2,
      nrow_full_na = 5,
      mean_production = 1e5
    )
  company_emissions <- company_emissions |>
    ungroup()
  return(company_emissions)
}

company_activities <- generate_company_activities()
company_emissions <- generate_company_emissions(company_activities)

company_activities <- company_activities |>
  dplyr::rename(activity_unit = .data$ald_production_unit)

company_emissions <- company_emissions |>
  dplyr::rename(activity_unit = .data$emissions_factor_unit)

readr::write_csv(company_activities, mock_filepaths$company_activities)
readr::write_csv(company_emissions, mock_filepaths$company_emissions)

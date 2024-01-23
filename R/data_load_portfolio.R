#' Title
#'
#' @param portfolio_data_path
#'
#' @return
#' @export
#'
#' @examples
load_portfolio_data <- function(portfolio_data_path) {
  if (!is.null(portfolio_data_path)) {
    portfolio_data <- readr::read_csv(
      portfolio_data_path,
      col_types = readr::cols_only(
        portfolio_id = "c",
        company_id = "c",
        company_name = "c",
        asset_type = "c",
        ald_sector = "c",
        ald_business_unit = "c",
        ald_location = "c",
        exposure_value_usd = "d",
        expiration_date = readr::col_date(format = "%Y-%m-%d"),
        loss_given_default = "d",
        pd_portfolio = "d"
      )
    )
  } else {
    portfolio_data <- tibble::tibble(
      portfolio_id = character(),
      asset_type = character(),
      company_id = character(),
      company_name = character(),
      ald_sector = character(),
      ald_business_unit = character(),
      exposure_value_usd = double(),
      expiration_date = date(),
      loss_given_default = double(),
      pd_portfolio = double()
    )
  }
  return(portfolio_data)
}


#' Title
#'
#' @param portfolio_data
#' @param maturity_month_term_bridge
#' @param start_year
#'
#' @return
#' @export
#'
#' @examples
map_portfolio_maturity_to_term <-
  function(portfolio_data,
           maturity_month_term_bridge,
           trisk_start_year) {
    min_maturity_month <-
      min(maturity_month_term_bridge$portfolio_maturity_month)
    max_maturity_month <-
      max(maturity_month_term_bridge$portfolio_maturity_month)

    start_date <- as.Date(paste0(trisk_start_year, "-01-01"))

    portfolio_data <- portfolio_data |>
      dplyr::mutate(
        portfolio_maturity_month = lubridate::interval(.env$start_date, .data$expiration_date) %/% months(1)
      ) |>
      dplyr::mutate(
        portfolio_maturity_month = dplyr::if_else(
          .data$portfolio_maturity_month < .env$min_maturity_month,
          .env$min_maturity_month,
          .data$portfolio_maturity_month
        )
      ) |>
      dplyr::mutate(
        portfolio_maturity_month = dplyr::if_else(
          .data$portfolio_maturity_month > .env$max_maturity_month,
          .env$max_maturity_month,
          .data$portfolio_maturity_month
        )
      ) |>
      dplyr::select(-c(.data$expiration_date))


    # Create the column term
    portfolio_data <- portfolio_data |>
      dplyr::left_join(maturity_month_term_bridge, by = "portfolio_maturity_month") |>
      dplyr::select(-c(.data$portfolio_maturity_month))


    # replace term by 5 when the asset is not expected to have a term
    portfolio_data <- portfolio_data |>
      dplyr::mutate(term = dplyr::if_else(asset_type == "fixed_income", .data$term, 5))

    return(portfolio_data)
  }



#' Title
#'
#' @param portfolio_data
#' @param group_cols
#'
#' @return
#'
#' @examples
aggregate_portfolio_facts <- function(portfolio_data, group_cols) {
  portfolio_data <- portfolio_data |>
    dplyr::group_by_at(group_cols) |>
    dplyr::summarize(
      exposure_value_usd = sum(.data$exposure_value_usd),
      loss_given_default = stats::median(loss_given_default, na.rm = T),
      pd_portfolio = stats::median(.data$pd_portfolio, na.rm = T),
      .groups = "drop"
    )

  return(portfolio_data)
}

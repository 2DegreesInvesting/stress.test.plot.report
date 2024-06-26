
#' Title
#'
#' @param portfolio_data portfolio_data
#' @param granularity granularity
#' @param param_cols param_cols
#' @param trisk_start_year trisk_start_year
#'
#'
#' @export
main_load_portfolio_data <-
  function(portfolio_data,
           granularity,
           trisk_start_year,
           param_cols = c("portfolio_id", "term", "asset_type")
           ) {
    group_cols <- unique(c(granularity, param_cols))

    portfolio_data <- portfolio_data |>
      map_portfolio_maturity_to_term(
        trisk_start_year = trisk_start_year
      ) |>
      aggregate_portfolio_facts(group_cols = group_cols)

    return(portfolio_data)
  }



#' Title
#'
#' @param portfolio_data_path
#'
load_portfolio_data <- function(portfolio_data_path=NULL) {
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
        expiration_date = "c",
        loss_given_default = "d",
        pd_portfolio = "d"
      )
    ) %>%
      convert_date_column(colname="expiration_date")

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


#' Convert column year to term. 1 term is equal to 2.5 years
#'
#' @param trisk_start_year trisk_start_year
#' @param portfolio_data portfolio_data
#'
map_portfolio_maturity_to_term <-
  function(portfolio_data,
           trisk_start_year) {
    start_year_exists <- !(is.na(trisk_start_year) | is.null(trisk_start_year))

    if (start_year_exists) {
      start_date <- as.Date(paste0(trisk_start_year, "-01-01"))

      #.TODO ADD A CHECK VALIDATING THE CONVERSION TO DATE WITH NO NAs
      portfolio_data <- portfolio_data |>
        dplyr::mutate(
          expiration_date = as.Date(.data$expiration_date),
          portfolio_maturity_month = lubridate::interval(.env$start_date, .data$expiration_date) %/% months(1),
          term = ceiling((.data$portfolio_maturity_month / 12) * 0.4) # 1 term is equal to 2.5 years
        ) |>
        dplyr::select(-c(portfolio_maturity_month))
    } else {
      # replace term by 1 if there's no start year,
      # which is a consequence of the crispy data being empty
      portfolio_data <- portfolio_data |>
        dplyr::mutate(term = 1)
    }

    # replace term by 1 if asset type is equity
    portfolio_data <- portfolio_data |>
      dplyr::mutate(
        term = dplyr::if_else(is.na(.data$term), 1, .data$term),
        portfolio_id=dplyr::if_else(is.na(.data$portfolio_id), "1", .data$portfolio_id))

    return(portfolio_data)
  }



#' Title
#'
#' @param portfolio_data portfolio_data
#' @param group_cols group_cols
#'
#'
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


#' Validate and conversion to date
#'
#' Define a function to check date format and convert
#'
#' @param data data
#' @param date_col_name date_col_name
#'
convert_date_column <- function(data_frame, colname) {
  # Check if the column exists in the dataframe
  if (!colname %in% names(data_frame)) {
    stop("Column not found in the dataframe")
  }

  # Use lubridate::ymd() to parse dates; invalid dates become NA
  data_frame[[colname]] <- lubridate::ymd(data_frame[[colname]], quiet = TRUE)

  # Check if there were any parsing failures (i.e., NAs introduced)
  if (any(is.na(data_frame[[colname]]))) {
    warning("Some dates were not in the correct 'YYYY-mm-dd' format and have been converted to NA")
  }

  return(data_frame)
}

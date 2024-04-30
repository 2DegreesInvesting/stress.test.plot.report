#' Data load function to generate plots
#'
#' @description
#'  The dataframe in output of this function should always be
#'  the one used as input for the plots preprocessing functions
#'  Dates should match the format YYYY-mm-dd
#'
#' @param crispy_outputs_dir crispy_outputs_dir
#' @param portfolio_data_path portfolio_data_path
#'
#' @export
#'
load_input_plots_data_from_files <-
  function(crispy_outputs_dir, portfolio_data_path = NULL,...) {

    multi_crispy_data <-
      load_multiple_crispy(crispy_outputs_dir = crispy_outputs_dir)

    portfolio_data <- load_portfolio_data(portfolio_data_path=portfolio_data_path)

    analysis_data <- load_input_plots_data_from_tibble(
      multi_crispy_data=multi_crispy_data,
      portfolio_data=portfolio_data, ...)

    return(analysis_data)
  }

#' Data load function to generate plots
#'
#' @description
#'  The dataframe in output of this function should always be
#'  the one used as input for the plots preprocessing functions
#'
#' @param granularity granularity
#' @param trisk_start_year (default) sets to the earliest year of multi_cripy_data
#' @param multi_crispy_data multi_crispy_data
#' @param portfolio_data portfolio_data
#' @param filter_outliers filter_outliers
#'
#' @export
#'
load_input_plots_data_from_tibble <-
  function(multi_crispy_data,
           portfolio_data = NULL,
           granularity = c("company_id", "company_name", "ald_sector", "ald_business_unit"),
           filter_outliers = FALSE) {

    raw_crispy_columns <- colnames(multi_crispy_data)

    multi_crispy_data <- multi_crispy_data |>
      main_load_multi_crispy_data(
        granularity = granularity,
        filter_outliers = filter_outliers
              )

    # start year must be unique
    stopifnot(length(unique(multi_crispy_data$start_year)) <= 1)
    trisk_start_year <- unique(multi_crispy_data$start_year)[1]

    if (!is.null(portfolio_data)){
      portfolio_data <- portfolio_data |>
        main_load_portfolio_data(
          granularity = granularity,
          trisk_start_year = trisk_start_year)
    } else{
      portfolio_data <- load_portfolio_data(portfolio_data_path=NULL)

      # drop_cols contains column that could be used as granularity but are dropped
      # TODO TEST columns that could be used as granularity are columns who are shared between dataframse
      shared_index  <-  dplyr::intersect(raw_crispy_columns, colnames(portfolio_data))
      drop_cols <- dplyr::setdiff(shared_index, granularity)
      portfolio_data <- portfolio_data |>
        dplyr::select_at(colnames(portfolio_data)[!colnames(portfolio_data) %in% drop_cols])
    }

    portfolio_crispy_merge_cols <- dplyr::intersect(
      c(granularity, "term"),
      dplyr::intersect(
        colnames(multi_crispy_data), colnames(portfolio_data)))
    stopifnot(length(portfolio_crispy_merge_cols) > 0)

    analysis_data <-
      main_load_analysis_data(
        portfolio_data = portfolio_data,
        multi_crispy_data = multi_crispy_data,
        portfolio_crispy_merge_cols = portfolio_crispy_merge_cols
      )

    return(analysis_data)
  }


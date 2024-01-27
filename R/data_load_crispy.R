load_multiple_crispy <- function(crispy_outputs_dir, max_granularity) {
  # Required Libraries

  # Get file paths
  files_path <- list.files(
    path = crispy_outputs_dir,
    pattern = "^crispy_output_(.*).csv",
    recursive = TRUE,
    full.names = TRUE
  )

  # Load all files into a list and add a run_id column for each dataframe
  data_list <- purrr::map(files_path, function(fp) {
    df <- readr::read_csv(fp,
      col_types = readr::cols_only(
        run_id = "c",
        term = "d",
        scenario_geography = "c",
        company_id = "c",
        company_name = "c",
        ald_sector = "c",
        ald_business_unit = "c",
        roll_up_type = "c",
        baseline_scenario = "c",
        shock_scenario = "c",
        risk_free_rate = "d",
        discount_rate = "d",
        div_netprofit_prop_coef = "d",
        growth_rate = "d",
        shock_year = "d",
        carbon_price_model = "c",
        market_passthrough = "d",
        financial_stimulus = "d",
        start_year = "d",
        net_present_value_baseline = "d",
        net_present_value_shock = "d",
        pd_baseline = "d",
        pd_shock = "d"
      )
    )
  })

  multi_crispy_data <- dplyr::bind_rows(data_list)

  return(multi_crispy_data)
}




#' Title
#'
#' # TODO FIND CLOSEST COMPANY IF group_cols=NULL
#'
#' @param multi_crispy multi_crispy
#' @param group_cols group_cols
#' @param param_cols TODO the parameters should be stored in a dataframe other than crispy and use the run_id as key to join
#'
aggregate_crispy_facts <- function(multi_crispy, group_cols) {
  multi_crispy <- multi_crispy |>
    dplyr::group_by_at(group_cols) |>
    dplyr::summarise(
      net_present_value_baseline = sum(net_present_value_baseline, na.rm = T),
      net_present_value_shock = sum(net_present_value_shock, na.rm = T),
      pd_baseline = stats::median(pd_baseline, na.rm = T),
      pd_shock = stats::median(pd_shock, na.rm = T),
      .groups = "drop"
    )
  return(multi_crispy)
}



#' Title
#'
#' @description
#' Function to remove outliers based on z-score
#'
#'
#' @param df df
#' @param column column name
#' @param max_zscore max_zscore
#'
remove_outliers <- function(df, column, max_zscore = 3) {
  # Compute the mean and standard deviation of the column
  mean_value <- mean(df[[column]], na.rm = TRUE)
  sd_value <- sd(df[[column]], na.rm = TRUE)

  # Calculate the Z-scores for the column
  z_scores <- (df[[column]] - mean_value) / sd_value

  outlier_companies <- unique(df[abs(z_scores) > max_zscore, "company_id"]) |>
    dplyr::pull()

  # Filter out rows where the absolute z-score is greater than 3
  df_filtered <- df |>
    dplyr::filter(!(.data$company_id %in% outlier_companies))

  return(df_filtered)
}

#' Title
#'
#' @param multi_crispy multi_crispy
#' @param group_cols group_cols
#'
remove_outliers_per_group <- function(multi_crispy, group_cols) {
  multi_crispy <- multi_crispy |>
    dplyr::group_by_at(group_cols) |>
    dplyr::group_modify(~ remove_outliers(.x, column = "crispy_perc_value_change")) |>
    dplyr::ungroup()
  return(multi_crispy)
}

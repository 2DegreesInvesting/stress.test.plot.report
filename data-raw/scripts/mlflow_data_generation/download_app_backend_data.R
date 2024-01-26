box::use(
  data - raw / scripts / constant[
    mlflow_python_bin, mlflow_bin, mlflow_uri, exp_name, artifact_names, mlflow_download_dir
  ],
  data - raw / scripts / constant[
    backend_crispy_data_path,
    backend_trajectories_data_path,
    backend_trisk_run_data_path,
    max_trisk_granularity
  ],
  data - raw / scripts / mlflow_mgmt / mlflow_data_collect[download_mlflow_search_result]
)

Sys.setenv(
  MLFLOW_PYTHON_BIN = mlflow_python_bin,
  MLFLOW_BIN = mlflow_bin
)


mlflow::mlflow_set_tracking_uri(uri = mlflow_uri)
mlflow::mlflow_client()
experiment <- mlflow::mlflow_get_experiment(name = exp_name)
experiment_id <- experiment[[1, "experiment_id"]]

all_runs <-
  mlflow::mlflow_search_runs(
    filter = "tags.LOG_STATUS = 'SUCCESS'",
    experiment_ids = as.character(experiment_id)
  )


### CRISPY COLLECT

download_mlflow_search_result(mlflow_uri, exp_name, all_runs, mlflow_download_dir, output_filename = "crispy_output.csv")

multi_crispy_data <- stress.test.plot.report::main_load_multi_crispy_data(
  crispy_outputs_dir = mlflow_download_dir,
  max_trisk_granularity = max_trisk_granularity
)
multi_crispy_data |>
  dplyr::select(
    run_id,
    run_name,
    ald_sector,
    term,
    net_present_value_baseline,
    net_present_value_shock,
    pd_baseline,
    pd_shock
  ) |>
  arrow::write_parquet(backend_crispy_data_path)

multi_crispy_data |>
  dplyr::distinct(
    run_id,
    roll_up_type,
    scenario_geography,
    baseline_scenario,
    shock_scenario,
    risk_free_rate,
    discount_rate,
    dividend_rate,
    growth_rate,
    shock_year
  ) |>
  arrow::write_parquet(backend_trisk_run_data_path)

### TRAJECTORIES COLLECT

download_mlflow_search_result(mlflow_uri, exp_name, all_runs, mlflow_download_dir, output_filename = "company_trajectories.csv")

files_path <- list.files(
  path = mlflow_download_dir,
  pattern = "^company_trajectories_.*.csv",
  recursive = TRUE,
  full.names = TRUE
)

# Load all files into a list and add a run_id column for each dataframe
data_list <- purrr::map(files_path, function(fp) {
  df <- readr::read_csv(fp) |>
    dplyr::mutate(
      run_id = sub("^company_trajectories_(.*)\\.csv", "\\1", basename(fp))
    )
})

multi_trajectories <- dplyr::bind_rows(data_list)

multi_trajectories |>
  dplyr::group_by(
    run_id,
    year,
    ald_sector,
    ald_business_unit
  ) |>
  dplyr::summarise(
    production_baseline_scenario = sum(production_baseline_scenario, na.rm = TRUE),
    production_target_scenario = sum(production_target_scenario, na.rm = TRUE),
    production_shock_scenario = sum(production_shock_scenario, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(run_id, ald_sector, ald_business_unit) |>
  dplyr::mutate(
    production_baseline_scenario = production_baseline_scenario / max(production_baseline_scenario),
    production_target_scenario = production_target_scenario / max(production_target_scenario),
    production_shock_scenario = production_shock_scenario / max(production_shock_scenario)
  ) |>
  dplyr::filter(year < max(year)) |>
  arrow::write_parquet(backend_trajectories_data_path)

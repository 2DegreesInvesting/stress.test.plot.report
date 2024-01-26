box::use(
  data - raw / scripts / mlflow_mgmt / trisk_mlflow[multirun_trisk_mlflow],
  scripts / constant[
    mlflow_python_bin, mlflow_bin,
    mlflow_uri, exp_name, trisk_input_path,
    trisk_output_path, artifact_names
  ],
  data - raw / scripts / constant[
    available_baseline_scenario, available_shock_scenario,
    available_discount_rate, available_risk_free_rate,
    available_growth_rate, available_shock_year,
    available_carbon_price_model, available_market_passthrough,
    available_scenario_geography
  ]
)

library(r2dii.climate.stress.test)

Sys.setenv(
  MLFLOW_PYTHON_BIN = mlflow_python_bin,
  MLFLOW_BIN = mlflow_bin
)


dir.create(fs::path(trisk_output_path), showWarnings = FALSE)
unlink(fs::path(trisk_output_path, "*"), recursive = TRUE)
# In a terminal opened in the project folder, execute this command to start a local mlflow server
# mlflow server --backend-store-uri ~/backup_mlflow_outputs/mlfow_outputs/mlruns --default-artifact-root ~/backup_mlflow_outputs/mlfow_outputs/mlartifacts --serve-artifacts --host 127.0.0.1 --port 5000

mlflow_python_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
mlflow_bin <-
  "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

Sys.setenv(
  MLFLOW_PYTHON_BIN = mlflow_python_bin,
  MLFLOW_BIN = mlflow_bin
)



scenario_pairs <-
  expand.grid(
    baseline_scenario = available_baseline_scenario,
    shock_scenario = available_shock_scenario
  )
# This block removes incompatible scenario pairs
# matching those where name match before the last "_"
scenario_pairs <-
  scenario_pairs |>
  dplyr::rowwise() |>
  dplyr::filter(paste(head(
    stringr::str_split(baseline_scenario, "_")[[1]], -1
  ), collapse = "_") == paste(head(
    stringr::str_split(shock_scenario, "_")[[1]], -1
  ), collapse = "_"))

# # TODO TEMP FIX REMOVE
# scenario_pairs <- dplyr::bind_rows(
#   scenario_pairs,
#   tibble::tribble(
#     ~baseline_scenario, ~shock_scenario,
#     "WEO2021_STEPS", "WEO2021_NZE_2050"
#   )
# )

params_grid <- list(
  discount_rate = available_discount_rate,
  # lgd = c(0.3, 0.45, 0.6, 0.75, 0.9),
  risk_free_rate = available_risk_free_rate,
  growth_rate = available_growth_rate,
  # div_netprofit_prop_coef = c(0.8, 0.85, 0.9, 0.95, 1),
  shock_year = available_shock_year,
  scenario_geography = available_scenario_geography,
  # settlement_factor = c(0, 0.3, 0.6, 1),
  # exp_share_damages_paid = c(0, 0.027, 0.1, 0.5, 1),
  # scc = c(0, 40, 400, 4000, 10000),
  # carbon_price_model = c(
  # "no_carbon_tax", "NZ2050", "NDC", "DN0", "B2DS")
  carbon_price_model = available_carbon_price_model,
  market_passthrough = available_market_passthrough

  # In a single experiment, do not change the names of parameters being tweaked in the param grid
)
multirun_trisk_mlflow(
  tracking_uri = mlflow_uri,
  experiment_name = exp_name,
  trisk_input_path = trisk_input_path,
  trisk_output_path = trisk_output_path,
  scenario_pairs = scenario_pairs,
  params_grid = params_grid,
  artifact_names = artifact_names
)

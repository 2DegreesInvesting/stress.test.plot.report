# MLFLOW DATA GENERATION CONSTANTS

mlflow_python_bin <- Sys.getenv("PYTHON_BIN") # python executable inside python .venv
mlflow_bin <- Sys.getenv("PYTHON_MLFLOW_BIN") # mlflow executable inside python .venv

# In a terminal opened in the project folder, execute this command to start a local mlflow server
# mlflow server --backend-store-uri ./data-raw/mlfow_outputs/mlruns --default-artifact-root ./data-raw/mlfow_outputs/mlartifacts --serve-artifacts --host 127.0.0.1 --port 5000

mlflow_uri <- "http://localhost:5000"
exp_name <- "app_crispys_test"

artifact_names <- c("crispy_output", "company_trajectories")

mlflow_download_dir <- fs::path("workspace", "app_crispys")



# GENERATE SYNTHETIC DATA CONSTANTS


mock_filepaths <- list(
  company_activities = here::here("data-raw", "mock_rawdata", "company_activities.csv"),
  company_emissions = here::here("data-raw", "mock_rawdata", "company_emissions.csv"),
  eikon_data = here::here("data-raw", "mock_rawdata", "eikon_data.csv"),
  trisk_input_path = here::here("data-raw", "mock_st_inputs"),
  abcd_stress_test_input = here::here("data-raw", "mock_st_inputs", "abcd_stress_test_input.csv"),
  prewrangled_financial_data_stress_test = here::here("data-raw", "mock_st_inputs", "prewrangled_financial_data_stress_test.csv"),
  trisk_output_path = here::here("data-raw", "mock_st_outputs"),
  portfolio_data = here::here("data-raw", "synthetic_csv")
)

production_types <- tibble::tribble(
  ~ald_sector, ~ald_business_unit, ~ald_production_unit, ~emissions_factor_unit,
  "Power", "RenewablesCap", "MW", "tCO2e",
  "Power", "RenewablesCap", "MWh", "tCO2e",
  "Shipping", "Freight", "dwt km", "tCO2",
  "Power", "GasCap", "MW", "tCO2e",
  "Power", "GasCap", "MWh", "tCO2e",
  "Power", "OilCap", "MW", "tCO2e",
  "Power", "OilCap", "MWh", "tCO2e",
  "Cement", "Integrated facility", "t cement", "tCO2e",
  "Shipping", "Passenger", "dwt km", "tCO2",
  "Oil&Gas", "Gas", "GJ", "tCO2e",
  "Oil&Gas", "Oil", "GJ", "tCO2e",
  "Power", "HydroCap", "MW", "tCO2e",
  "Power", "HydroCap", "MWh", "tCO2e",
  "Power", "CoalCap", "MW", "tCO2e",
  "Power", "CoalCap", "MWh", "tCO2e",
  "Aviation", "Freight", "tkm", "tCO2",
  "Aviation", "Passenger", "pkm", "tCO2",
  "Coal", "Coal", "t coal", "tCO2e",
  "Power", "NuclearCap", "MW", "tCO2e",
  "Power", "NuclearCap", "MWh", "tCO2e",
  "Automotive", "Electric", "# vehicles", "tCO2",
  "Automotive", "Hybrid", "# vehicles", "tCO2",
  "Automotive", "ICE", "# vehicles", "tCO2",
  "Steel", "Basic Oxygen Furnace", "t steel", "tCO2e",
  "Steel", "Electric Arc Furnace", "t steel", "tCO2e",
  "Automotive", "FuelCell", "# vehicles", "tCO2",
  "Steel", "Open Hearth Furnace", "t steel", "tCO2e"
)

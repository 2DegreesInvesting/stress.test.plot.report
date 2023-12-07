# mlflow_python_bin <-
#   "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/python"
# mlflow_bin <-
#   "/Users/bertrandgallice/opt/miniconda3/envs/mlflow_env/bin/mlflow"

# mlflow_uri <- "http://localhost:5000"
# experiment_name <- "cgfi_paper_overshoo_fix"

# output_dir <- here::here("data/trisk_runs")

# test_that("setup_r_mlflow works", {
#   setup_r_mlflow(mlflow_python_bin, mlflow_bin, mlflow_uri)
# })

# test_that("get_successful_experiment_runs works", {
#   all_runs <- get_successful_experiment_runs(experiment_name)
# })

# test_that("collect_and_aggregate_crispys works", {
#   all_crispy <- collect_and_aggregate_crispys(all_runs)
# })

# test_that("download_experiment_outputs works", {
#   download_experiment_outputs(all_runs, output_dir, unzip = T)
# })

read_csv_from_zipped_artifacts <- function(tracking_uri,
                                           experiment_name,
                                           run_id,
                                           dataframe_name) {
  mlflow::mlflow_set_tracking_uri(uri = tracking_uri)
  mlflow::mlflow_client()

  artifacts_path <-
    mlflow::mlflow_download_artifacts(path = "", run_id = run_id)
  f_conn <-
    unz(fs::path(artifacts_path, dataframe_name, ext = "zip"), csv_filename)
  artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
  return(artifact)
}


download_mlflow_search_result <- function(mlflow_uri, exp_name, all_runs, trisk_output_dir, output_filename) {
  if (dir.exists(trisk_output_dir)) {
    # Remove the .csv extension
    base_name <- sub("\\.csv$", "", output_filename)
    # Create the pattern
    pattern <- paste0(base_name, "_.+\\.csv")
    # List files with the specified pattern in the directory and subdirectories
    files_to_delete <- list.files(trisk_output_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
    file.remove(files_to_delete)
  }

  # Initializes the progress bar
  pb <- utils::txtProgressBar(
    min = 0, # Minimum value of the progress bar
    max = nrow(all_runs), # Maximum value of the progress bar
    style = 3, # Progress bar style (also available style = 1 and style = 2)
    width = 50, # Progress bar width. Defaults to getOption("width")
    char = "="
  ) # Character used to create the bar

  all_crispy <- NULL
  i <- 1
  for (run_id in all_runs[["run_uuid"]]) {
    crispy <- read_csv_from_zipped_artifacts(
      tracking_uri = mlflow_uri,
      experiment_name = exp_name,
      run_id = run_id,
      csv_filename = output_filename
    )

    trisk_run_output_dir <- fs::path(trisk_output_dir, run_id)
    dir.create(trisk_run_output_dir, showWarnings = FALSE, recursive = TRUE)
    crispy |> readr::write_csv(
      fs::path(
        trisk_run_output_dir,
        paste0(tools::file_path_sans_ext(output_filename), "_", run_id),
        ext = "csv"
      )
    )

    i <- i + 1
    utils::setTxtProgressBar(pb, i)
  }
}

# TODO REMOVE THIS SCRIPT AND REWRITE IN PYTHON
# ALL INFRA IN PYTHON


#' In a terminal opened in the project folder,
#' execute this command to start a local mlflow server :
#'  mlflow server --backend-store-uri ~/mlflow_outputs/mlruns --default-artifact-root ~/mlflow_outputs/mlartifacts --serve-artifacts --host 127.0.0.1 --port 5000
#' @param mlflow_python_bin
#' @param mlflow_bin
#' @param mlflow_uri
#'
#' @return
#' @export
#'
#' @examples
setup_r_mlflow <- function(mlflow_python_bin,
                           mlflow_bin,
                           mlflow_uri = "http://localhost:5000") {
  Sys.setenv(MLFLOW_PYTHON_BIN = mlflow_python_bin,
             MLFLOW_BIN = mlflow_bin)

  mlflow::mlflow_set_tracking_uri(uri = mlflow_uri)
  mlflow::mlflow_client()
}

#' Title
#'
#' @param experiment_name
#'
#' @return
#' @export
#'
#' @examples
get_successful_experiment_runs <- function(experiment_name) {
  experiment <- mlflow::mlflow_get_experiment(name = experiment_name)
  experiment_id <- experiment[[1, "experiment_id"]]

  all_runs <-
    mlflow::mlflow_search_runs(filter = "tags.LOG_STATUS = 'SUCCESS'",
                               experiment_ids = as.character(experiment_id))

  return(all_runs)

}


#' Crispy collect
#'
#' @param all_runs
#'
#' @return
#' @export
#'
#' @examples
collect_and_aggregate_crispys <- function(all_runs) {
  read_csv_from_zipped_artifacts <-
    function(run_id,
             csv_filename) {
      artifacts_path <-
        mlflow::mlflow_download_artifacts(path = "", run_id = run_id)
      f_conn <-
        unz(file.path(artifacts_path, "artifacts.zip"), csv_filename)
      artifact <- readr::read_csv(f_conn, show_col_types = FALSE)
      return(artifact)
    }


  all_crispy <- NULL
  for (run_id in all_runs[["run_uuid"]]) {
    crispy <- read_csv_from_zipped_artifacts(run_id = run_id,
                                             csv_filename = "crispy_output.csv")
    crispy <- crispy %>%
      dplyr::mutate(scenario_duo = paste(baseline_scenario, "&", shock_scenario, sep = ""))
    crispy <- crispy %>%
      dplyr::mutate(run_id = run_id)

    all_crispy <- dplyr::bind_rows(all_crispy, crispy)
  }
  return(all_crispy)
}



collect_artifacts <-
  function(output_dir,
            run_id,
            save_name) {
    dir.create(output_dir, showWarnings = F)
    artifacts_path <-
      mlflow::mlflow_download_artifacts(path = "", run_id = run_id)

    # TODO rewrite here with connection to cloud
    file.copy(from = file.path(artifacts_path, "artifacts.zip"),
              to = output_dir)
    file.rename(
      from = file.path(output_dir, "artifacts.zip"),
      to = file.path(output_dir, paste0(save_name, ".zip"))
    )
  }





#' Artifacts collect
#'
#' @param all_runs
#' @param output_dir
#' @param unzip
#'
#' @return
#' @export
#'
#' @examples
download_experiment_outputs <- function(all_runs,
                                        output_dir,
                                        unzip = F) {

  for (run_id in all_runs[["run_uuid"]]) {
    shock_scen <- (all_runs[all_runs$run_uuid == run_id,] %>%
                     dplyr::pull(params))[[1]] %>%
      dplyr::filter(key == "shock_scenario") %>%
      dplyr::pull(value)
    baseline_scen <- (all_runs[all_runs$run_uuid == run_id,] %>%
                        dplyr::pull(params))[[1]] %>%
      dplyr::filter(key == "baseline_scenario") %>%
      dplyr::pull(value)

    save_name = paste0(baseline_scen, "&", shock_scen)

    collect_artifacts(output_dir = output_dir,
                      run_id = run_id,
                      save_name = save_name)
  }

  if (unzip) {
    tryCatch({
      zipF <-
        list.files(path = output_dir,
                   pattern = "*.zip",
                   full.names = T)
      purrr:::walk(zipF,
                   ~ unzip(
                     zipfile = .x,
                     exdir =  stringr::str_sub(.x, end = -5L), # remove .zip to obtain a folder name
                     overwrite = T
                   ))
    },
    finally = {
      # delete zip files
      unlink(zipF)
    })

  }
}

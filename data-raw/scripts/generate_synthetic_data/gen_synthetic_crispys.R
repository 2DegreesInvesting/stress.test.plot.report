box::use(
  `data-raw` / scripts / constant[mock_filepaths],
  r2dii.climate.stress.test[...]
)


dir.create(fs::path(mock_filepaths$trisk_output_path), showWarnings = FALSE)
unlink(fs::path(mock_filepaths$trisk_output_path, "*"), recursive = TRUE)

####### RUN TRISK

run_trisk(
  input_path = mock_filepaths$trisk_input_path,
  output_path = mock_filepaths$trisk_output_path
)


run_trisk(
  input_path = mock_filepaths$trisk_input_path,
  output_path = mock_filepaths$trisk_output_path,
  baseline_scenario = "NGFS2023REMIND_NDC",
  shock_scenario = "NGFS2023REMIND_NZ2050"
)

run_trisk(
  input_path = mock_filepaths$trisk_input_path,
  output_path = mock_filepaths$trisk_output_path,
  baseline_scenario = "NGFS2023REMIND_NDC",
  shock_scenario = "NGFS2023REMIND_NZ2050",
  shock_year = 2030
)

run_trisk(
  input_path = mock_filepaths$trisk_input_path,
  output_path = mock_filepaths$trisk_output_path,
  baseline_scenario = "NGFS2023REMIND_NDC",
  shock_scenario = "NGFS2023REMIND_NZ2050",
  shock_year = 2033
)

run_trisk(
  input_path = mock_filepaths$trisk_input_path,
  output_path = mock_filepaths$trisk_output_path,
  baseline_scenario = "NGFS2023REMIND_NDC",
  shock_scenario = "NGFS2023REMIND_NZ2050",
  shock_year = 2035
)

run_trisk(
  input_path = mock_filepaths$trisk_input_path,
  output_path = mock_filepaths$trisk_output_path,
  baseline_scenario = "NGFS2023MESSAGE_NDC",
  shock_scenario = "NGFS2023MESSAGE_B2DS"
)

run_trisk(
  input_path = mock_filepaths$trisk_input_path,
  output_path = mock_filepaths$trisk_output_path,
  baseline_scenario = "Oxford2021_base",
  shock_scenario = "Oxford2021_fast"
)



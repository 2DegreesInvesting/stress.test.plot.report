source("constant.R")



get_path_to_root_from_test <- function() {
  wd_parts <- unlist(strsplit(getwd(), "/"))
  is_test_folder <-
    length(wd_parts) >= 3 &&
    all(tail(wd_parts, 2) == c("tests", "testthat"))
  if (is_test_folder) {
    return(file.path(getwd(), "..", ".."))
  } else{
    return(getwd())
  }
}

test_that("analysis_data loads_properly on ald_sector", {

  withr::with_dir(get_path_to_root_from_test(),
                  {
                    analysis_data <-
                      load_input_plots_data_from_files(
                        crispy_outputs_dir = MOCK_ST_OUTPUTS_PATH,
                        portfolio_data_path = MOCK_PORTFOLIO_DATA_PATH,
                        granularity = "ald_sector",
                        filter_outliers = TRUE
                      )
                  })

  expect_snapshot(analysis_data)
})


test_that("analysis_data loads_properly on ald_sector and business_unit",
          {
            withr::with_dir(get_path_to_root_from_test(),
                            {
                              analysis_data <-
                                load_input_plots_data_from_files(
                                  crispy_outputs_dir = MOCK_ST_OUTPUTS_PATH,
                                  portfolio_data_path = MOCK_PORTFOLIO_DATA_PATH,
                                  granularity = c("ald_sector", "ald_business_unit"),
                                  filter_outliers = TRUE
                                )
                            })
            expect_snapshot(analysis_data)
          })



test_that("main_data_load_trajectories_data_from_file loads_properly on ald_sector and business_unit",
          {
            withr::with_dir(get_path_to_root_from_test(),
                            {
                              analysis_data <-
                                main_data_load_trajectories_data_from_file(
                                  crispy_outputs_dir = MOCK_ST_OUTPUTS_PATH,
                                  granularity = c("ald_sector", "ald_business_unit")                                )
                            })
            expect_snapshot(analysis_data)
          })

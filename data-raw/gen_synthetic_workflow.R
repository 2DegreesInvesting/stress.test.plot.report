
print("=================== RUNNING gen_synthetic_company_data ===================")
source(here::here("data-raw", "scripts", "generate_synthetic_data", "gen_synthetic_company_data.R"))
rm(list = ls())

print("=================== RUNNING gen_synthetic_abcd ===================")
source(here::here("data-raw", "scripts", "generate_synthetic_data", "gen_synthetic_abcd.R"))
rm(list = ls())

print("=================== RUNNING gen_synthetic_eikon_data ===================")
source(here::here("data-raw", "scripts", "generate_synthetic_data", "gen_synthetic_eikon_data.R"))
rm(list = ls())

print("=================== RUNNING gen_synthetic_financial_data ===================")
source(here::here("data-raw", "scripts", "generate_synthetic_data", "gen_synthetic_financial_data.R"))
rm(list = ls())

print("=================== RUNNING gen_synthetic_financial_data ===================")
source(here::here("data-raw", "scripts", "generate_synthetic_data", "gen_synthetic_crispys.R"))
rm(list = ls())



#
#
# box::use(
#   fixtures/ synthetic_file_paths[mock_filepaths],
# )
#
#   synthetic_company_activities  <-   arrow::read_parquet(mock_filepaths$company_activities)
#   synthetic_company_emissions <- arrow::read_parquet(mock_filepaths$company_emissions)
#   synthetic_eikon_data <- arrow::read_parquet(mock_filepaths$eikon_data)
#
#   usethis::use_data(synthetic_company_activities, overwrite=TRUE)
#   usethis::use_data(synthetic_company_emissions, overwrite=TRUE)
#   usethis::use_data(synthetic_eikon_data, overwrite=TRUE)

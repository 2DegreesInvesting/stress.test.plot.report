
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


print("=================== RUNNING gen_synthetic_financial_data ===================")
source(here::here("data-raw", "scripts", "generate_synthetic_data", "generate_synthetic_portfolio_data.R"))
rm(list = ls())
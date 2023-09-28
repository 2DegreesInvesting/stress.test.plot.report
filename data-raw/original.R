## please open file within the stress test repo

devtools::load_all()
library(ggplot2)
library(dplyr)
library(r2dii.colours)
library(r2dii.plot)
library(r2dii.utils)
library(tidyverse)
library(scales)
library(cowplot)

################################################################################
# LOAD Exposure data

equity_input_path <- r2dii.utils::path_dropbox_2dii(
  "PortCheck_v2",
  "10_Projects",
  "CDI",
  "data",
  glue::glue("california_exposures_per_ar_id.csv")
)

equity_exposures <- readr::read_csv(
  equity_input_path,
  col_types = readr::cols_only(
    holding_id = "c",
    insurance_category = "c",
    value_usd = "d",
    asset_type = "c",
    ar_company_id = "d"
  )
)

#quick equity manipulation
equity_exposures <-
  equity_exposures %>%
  dplyr::filter(asset_type == "Equity") %>%
  dplyr::select(-c("asset_type"))

## add cpr prefix pre-matching
names(equity_exposures) <-
  paste0("cpr_", names(equity_exposures))


## LOAD Bond data

bonds_input_path <- r2dii.utils::path_dropbox_2dii(
  "PortCheck_v2",
  "10_Projects",
  "CDI",
  "data",
  glue::glue("matched_bond_data_meta.csv") ##here change meta or not meta portfolio included
)

bond_data <- readr::read_csv(
  bonds_input_path,
  col_types = readr::cols_only(
    isin = "c",
    insurance_category = "c",
    asset_type = "c",
    ar_company_id = "d",
    value_usd = "d",
    first_maturity = "c",
    term = "d")
)
################################################################################

# LOAD Default WEO run

input_path_default <- r2dii.utils::path_dropbox_2dii(
  "PortCheck_v2",
  "10_Projects",
  "CDI",
  "data",
  "2023-03-28_18-21-04_standard",
  glue::glue("crispy_output_standard.csv")
)

crispy_output_standard <- readr::read_csv(
  input_path_default,
  col_types = readr::cols_only(
    company_name = "c",
    sector = "c",
    business_unit = "c",
    roll_up_type = "c",
    scenario_geography = "c",
    baseline_scenario = "c",
    shock_scenario = "c",
    lgd = "d",
    risk_free_rate = "d",
    discount_rate = "d",
    dividend_rate = "d",
    growth_rate = "d",
    shock_year = "d",
    net_present_value_baseline = "d",
    net_present_value_shock = "d",
    net_present_value_difference = "d",
    term = "d",
    pd_baseline = "d",
    pd_shock = "d",
    pd_difference = "d",
    id = "d"
  )
)

################################################################################

input_path_sensitivity <- r2dii.utils::path_dropbox_2dii(
  "PortCheck_v2",
  "10_Projects",
  "CDI",
  "data",
  "ST_outputs"
)

# LOAD Default WEO run

crispy_output_shock_year <- readr::read_csv(
  input_path_default,
  col_types = readr::cols_only(
    company_name = "c",
    sector = "c",
    business_unit = "c",
    roll_up_type = "c",
    scenario_geography = "c",
    baseline_scenario = "c",
    shock_scenario = "c",
    lgd = "d",
    risk_free_rate = "d",
    discount_rate = "d",
    dividend_rate = "d",
    growth_rate = "d",
    shock_year = "d",
    net_present_value_baseline = "d",
    net_present_value_shock = "d",
    net_present_value_difference = "d",
    term = "d",
    pd_baseline = "d",
    pd_shock = "d",
    pd_difference = "d",
    id = "d"
  )
)


# LOAD in different crispy runs
output_paths <- c(
  r2dii.utils::path_dropbox_2dii("PortCheck_v2", "10_Projects", "CDI", "data", "CDI_runs"))

dataset_files <- list.files(path = output_paths, pattern = "crispy_", recursive = TRUE, full.names = TRUE)

data_list <- list()

for (file in dataset_files) {
  data <- read.csv(file)

  # file_name <- basename(dirname(file))
  # file_name <- gsub("[0-9]", "", file_name)
  parent_dir <- basename(dirname(file))
  parent_modified_filename <- substring(parent_dir, 27)
  sub_dir <- basename((file))
  sub_modified_filename <- substring(sub_dir, 1, 14)

  # Create the modified filename with both parent and subdirectory names
  modified_filename <- paste0(sub_modified_filename, parent_modified_filename)

  data_list[[modified_filename]] <- data

}

# LOAD in GECO runs

output_paths_geco <- c(
  r2dii.utils::path_dropbox_2dii("PortCheck_v2", "10_Projects", "CDI", "data", "CDI_runs", "Geco"))

dataset_files_geco <- list.files(path = output_paths_geco, pattern = "crispy_", recursive = TRUE, full.names = TRUE)

data_list_geco <- list()

for (file in dataset_files_geco) {
  data <- read.csv(file)

  # file_name <- basename(dirname(file))
  # file_name <- gsub("[0-9]", "", file_name)
  parent_dir <- basename(dirname(file))
  parent_modified_filename <- substring(parent_dir, 27)
  sub_dir <- basename((file))
  sub_modified_filename <- substring(sub_dir, 1, 14)

  # Create the modified filename with both parent and subdirectory names
  modified_filename <- paste0(sub_modified_filename, parent_modified_filename)

  data_list_geco[[modified_filename]] <- data

}


##categories can own the same holding_id

crispy_cpr_company_id <- map(data_list, ~ .x %>% dplyr::filter(roll_up_type == "equity_ownership") %>%
                               dplyr::filter(term == "1") %>%
                               dplyr::select(-c("term")) %>%
                               dplyr::inner_join(equity_exposures,by = c("id" = "cpr_ar_company_id")) %>%
                               dplyr::group_by(cpr_insurance_category, id) %>%
                               dplyr::mutate(sum_cpr_value_usd = sum(cpr_value_usd, na.rm = TRUE)) %>%
                               dplyr::ungroup())


######################################################################################################################################################

### Sector Weights
### picking only the relevant scenarios for the weights

WEO_SY <- data_list$crispy_output__shock_year_WEO_global
GCAM_SY <- data_list$crispy_output__shock_year_NGFS_global_GCAM
REMIND_SY <- data_list$crispy_output__shock_year_NGFS_global_REMIND
WEO_NA_SY <- data_list$crispy_output__shock_year_WEO_NorthAmerica
GCAMTax_SY <- data_list$crispy_output__shock_year_NGFS_global_GCAM_carbon_tax

WEO_SY <- WEO_SY[WEO_SY$shock_year==2030 & WEO_SY$term==1,]
GCAM_SY <- GCAM_SY[GCAM_SY$shock_year==2030 & GCAM_SY$term==1,]
REMIND_SY <- REMIND_SY[REMIND_SY$shock_year==2030 & REMIND_SY$term==1,]
WEO_NA_SY <-WEO_NA_SY[WEO_NA_SY$shock_year==2030 & WEO_NA_SY$term==1,]
GCAMTax_SY <- GCAMTax_SY[GCAMTax_SY$shock_year==2030 & GCAMTax_SY$term==1,]

## calculating the sector weights
## These are the weihts used for EL plots and calculations

## Weights WEO
total_NPV_WEO <- WEO_SY %>%
  group_by(company_name, id) %>%
  summarize(sum_NPV_total = sum(net_present_value_baseline))

sector_NPV_WEO <- WEO_SY %>%
  group_by(company_name, sector, id) %>%
  summarize(sum_NPV_sector = sum(net_present_value_baseline))

scenario_weights_WEO <- merge(sector_NPV_WEO, total_NPV_WEO, by= c("company_name", "id"))

##final weights WEO
sector_weights_WEO <- scenario_weights_WEO %>%
  group_by(company_name, id, sector) %>%
  mutate(sector_weight = sum_NPV_sector / sum_NPV_total)

## GCAM
total_NPV_GCAM <- GCAM_SY %>%
  group_by(company_name, id) %>%
  summarize(sum_NPV_total = sum(net_present_value_baseline))

sector_NPV_GCAM <- GCAM_SY %>%
  group_by(company_name, sector, id) %>%
  summarize(sum_NPV_sector = sum(net_present_value_baseline))

scenario_weights_GCAM <- merge(sector_NPV_GCAM, total_NPV_GCAM, by= c("company_name", "id"))

## final weights GCAM
sector_weights_GCAM <- scenario_weights_GCAM %>%
  group_by(company_name, id, sector) %>%
  mutate(sector_weight = sum_NPV_sector / sum_NPV_total)

## GCAM Ctax
total_NPV_GCAMTax <- GCAMTax_SY %>%
  group_by(company_name, id) %>%
  summarize(sum_NPV_total = sum(net_present_value_baseline))

sector_NPV_GCAMTax <- GCAMTax_SY %>%
  group_by(company_name, sector, id) %>%
  summarize(sum_NPV_sector = sum(net_present_value_baseline))

scenario_weights_GCAMTax <- merge(sector_NPV_GCAMTax, total_NPV_GCAMTax, by= c("company_name", "id"))

## final weights GCAMctax
sector_weights_GCAMTax <- scenario_weights_GCAMTax %>%
  group_by(company_name, id, sector) %>%
  mutate(sector_weight = sum_NPV_sector / sum_NPV_total)

## Weights WEO NA
total_NPV_WEONA <- WEO_NA_SY %>%
  group_by(company_name, id) %>%
  summarize(sum_NPV_total = sum(net_present_value_baseline))

sector_NPV_WEONA <- WEO_NA_SY %>%
  group_by(company_name, sector, id) %>%
  summarize(sum_NPV_sector = sum(net_present_value_baseline))

scenario_weights_WEONA <- merge(sector_NPV_WEONA, total_NPV_WEONA, by= c("company_name", "id"))

##final weights WEONA
sector_weights_WEONA <- scenario_weights_WEONA %>%
  group_by(company_name, id, sector) %>%
  mutate(sector_weight = sum_NPV_sector / sum_NPV_total)

## REMIND
## calculating the weights REMIND
total_NPV_REMIND <- REMIND_SY %>%
  group_by(company_name, id) %>%
  summarize(sum_NPV_total = sum(net_present_value_baseline))

sector_NPV_REMIND <- REMIND_SY %>%
  group_by(company_name, sector, id) %>%
  summarize(sum_NPV_sector = sum(net_present_value_baseline))

scenario_weights_REMIND <- merge(sector_NPV_REMIND, total_NPV_REMIND, by= c("company_name", "id"))

## final weights REMIND
sector_weights_REMIND <- scenario_weights_REMIND %>%
  group_by(company_name, id, sector) %>%
  mutate(sector_weight = sum_NPV_sector / sum_NPV_total)

###########

### Loading in data
REMIND_DR <- data_list$crispy_output__discount_rate_NGFS_global_REMIND
WEO_DR <- data_list$crispy_output__discount_rate_WEO_global
GCAM_DR <- data_list$crispy_output__discount_rate_NGFS_global_GCAM
WEO_NA_DR <- data_list$crispy_output_discount_rate_WEO_NorthAmerica
REMIND_SY <- data_list$crispy_output__shock_year_NGFS_global_REMIND
WEO_SY <- data_list$crispy_output__shock_year_WEO_global
GCAM_SY <- data_list$crispy_output__shock_year_NGFS_global_GCAM
WEO_NA_SY <- data_list$crispy_output__shock_year_WEO_NorthAmerica
GCAMTax_SY <- data_list$crispy_output__shock_year_NGFS_global_GCAM_carbon_tax
GCAMTax_DR <- data_list$crispy_output__discount_rate_NGFS_global_GCAM_carbon_tax


## plotting calibrations
width <- 18
height <- 10
dpi <- 300


####
colnames(bond_data)[colnames(bond_data) == "ar_company_id"] <- "id"
# Define the desired order of insurance categories
desired_order <- c("fraternal", "life", "health", "p_n_c", "meta")
color_gradient <- c("#FFC0C0", "#FF8888", "#FF6666", "#FF4444", "#FF2222", "#FF0000")

# List of scenario dataframes
Scenarios_Sy <- list(WEO_SY = WEO_SY, WEO_NA_SY = WEO_NA_SY, GCAM_SY = GCAM_SY, REMIND_SY = REMIND_SY, GCAMTax_SY = GCAMTax_SY)


################################## MEAD PD DIFF SY PLOT
# Loop through scenarios
for (scenario_name in names(Scenarios_Sy)) {
  scenario_data <- Scenarios_Sy[[scenario_name]]

  # Merge data
  Shock_Year_bond_long <- merge(bond_data, scenario_data, by=c("term", "id"))

  # Calculate mean pd difference
  mean_shockyear <- Shock_Year_bond_long %>%
    group_by(insurance_category, sector, shock_year) %>%
    summarise(mean_pd_difference = mean(pd_difference))

  # Reorder levels of insurance_category based on desired order
  mean_shockyear$insurance_category <- factor(mean_shockyear$insurance_category, levels = desired_order)

  # Remove "SY" from scenario name for plot title
  scenario_name_for_title <- gsub("_SY", "", scenario_name)

  # Create plot
  plotsy <- ggplot(data = mean_shockyear, aes(x = shock_year, group = factor(shock_year), y = mean_pd_difference, fill = mean_pd_difference)) +
    geom_bar(stat = "identity") +
    scale_fill_gradientn(colors = color_gradient,
                         limits = c(min(mean_shockyear$mean_pd_difference), max(mean_shockyear$mean_pd_difference)),
                         breaks = c(min(mean_shockyear$mean_pd_difference), 0, max(mean_shockyear$mean_pd_difference)),
                         labels = scales::comma,
                         name = "Mean pd_difference") +
    facet_grid(insurance_category ~ sector, scales = "free_y") +  # Allow separate scales for y-axis
    theme_2dii() +
    labs(x = "Shock_Year", y = "Mean pd_difference", title = paste("Mean PD Difference by Shock Year -", scenario_name_for_title)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 28),  # Adjust title size
          axis.text = element_text(size = 23),  # Adjust tick label size
          axis.title.x = element_text(size = 26),  # Adjust x-axis label size
          axis.title.y = element_text(size = 26),  # Adjust y-axis label size
          strip.text = element_text(size = 24),
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.2),
          legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20)) +  # Adjust legend title size
    scale_x_continuous(breaks = c(2026, 2030, 2034))

  # Output Path and filename
  output_path <- "/Users/2diigermany/Desktop/Work/CDI/new plots_last/SY/PD Diff/"
  filename <- paste0(output_path, "PDdiff_Shock_year_", scenario_name, ".png")

  # Save the plot
  ggsave(filename, plot = plotsy, width = width, height = height, dpi = dpi)
}

########################################################################
########################################################################
########################################################################
## Expected Loss total
library(ggplot2)
library(dplyr)

# List of scenario names and corresponding sector weights dataframes
scenarios_sector_weights <- list(
  WEO_SY = sector_weights_WEO,
  WEO_NA_SY = sector_weights_WEONA,
  GCAM_SY = sector_weights_GCAM,
  REMIND_SY = sector_weights_REMIND,
  GCAMTax_SY = sector_weights_GCAMTax
)
## EL PLOT LOOP SY
# Loop through scenarios and corresponding sector weights
for (scenario_name in names(scenarios_sector_weights)) {
  sector_weights_df <- scenarios_sector_weights[[scenario_name]]

  # Identify common columns for merging
  common_columns <- intersect(c("id", "company_name", "sector"), colnames(sector_weights_df))

  # Merge data
  Shock_Year_bond_long <- merge(bond_data, Scenarios_Sy[[scenario_name]], by = c("term", "id"))
  Shock_Year_bond_long <- merge(Shock_Year_bond_long, sector_weights_df, by = common_columns)

  # Calculate EL with 75% LGD
  Shock_Year_bond_long$EL75 <- Shock_Year_bond_long$pd_difference * Shock_Year_bond_long$value_usd * Shock_Year_bond_long$sector_weight * 0.75

  # Sum over sector and portfolio
  EL_sum <- Shock_Year_bond_long %>%
    group_by(insurance_category, sector, shock_year) %>%
    summarise(SumEL75 = sum(EL75))

  # Remove "_SY" from scenario name for plot title
  scenario_name_for_title <- gsub("_SY", "", scenario_name)

  # Reorder levels of insurance_category based on desired order
  desired_order <- c("fraternal", "life", "health", "p_n_c", "meta")
  EL_sum$insurance_category <- factor(EL_sum$insurance_category, levels = desired_order)

  # Create plot
  plotEL_sy <- ggplot(data = EL_sum, aes(x = shock_year, group = factor(shock_year), y = SumEL75, fill = SumEL75)) +
    geom_bar(stat = "identity") +
    scale_fill_gradientn(
      colors = color_gradient,
      limits = c(min(EL_sum$SumEL75), max(EL_sum$SumEL75)),
      breaks = c(min(EL_sum$SumEL75), 0, max(EL_sum$SumEL75)),
      labels = scales::comma,
      name = "Sum of EL"
    ) +
    facet_grid(insurance_category ~ sector ) +
    theme_2dii() +
    labs(x = "Shock Year", y =  "Sum of EL", title = paste("Sum of EL per Insurance and Sector - 75% LGD - Shock Year -", scenario_name_for_title)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 28),
          axis.text = element_text(size = 23),
          axis.title.x = element_text(size = 26),
          axis.title.y = element_text(size = 26),
          strip.text = element_text(size = 24),
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.2),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)) +
    scale_y_continuous(
      labels = scales::label_number_si(scale = 1e-9, suffix = " bn")
    ) +
    scale_x_continuous(breaks = c(2026, 2030, 2034))

  # Output Path and filename
  output_path <- "/Users/2diigermany/Desktop/Work/CDI/new plots_last/SY/EL/"
  filename <- paste0(output_path, "EL_Total_Shock_year_", scenario_name, ".png")

  # Save the plot
  ggsave(filename, plot = plotEL_sy, width = width, height = height, dpi = dpi)
}

########
#######
#########
##loop for each insurance category: EL


# List of scenario names and corresponding sector weights dataframes
scenarios_sector_weights <- list(
  WEO_SY = sector_weights_WEO,
  WEO_NA_SY = sector_weights_WEONA,
  GCAM_SY = sector_weights_GCAM,
  REMIND_SY = sector_weights_REMIND,
  GCAMTax_SY = sector_weights_GCAMTax
)

# List of insurance categories
insurance_categories <- c("fraternal", "life", "health", "p_n_c", "meta")

# EL for each portfolio loop
#Loop through scenarios and corresponding sector weights
for (scenario_name in names(scenarios_sector_weights)) {
  sector_weights_df <- scenarios_sector_weights[[scenario_name]]

  # Identify common columns for merging
  common_columns <- intersect(c("id", "company_name", "sector"), colnames(sector_weights_df))

  # Merge data
  Shock_Year_bond_long <- merge(bond_data, Scenarios_Sy[[scenario_name]], by = c("term", "id"))
  Shock_Year_bond_long <- merge(Shock_Year_bond_long, sector_weights_df, by = common_columns)

  # Calculate EL with 75% LGD
  Shock_Year_bond_long$EL75 <- Shock_Year_bond_long$pd_difference * Shock_Year_bond_long$value_usd * Shock_Year_bond_long$sector_weight * 0.75

  # Sum over sector and portfolio
  EL_sum <- Shock_Year_bond_long %>%
    group_by(insurance_category, sector, shock_year) %>%
    summarise(SumEL75 = sum(EL75))

  # Remove "_SY" from scenario name for plot title
  scenario_name_for_title <- gsub("_SY", "", scenario_name)

  for (ins in insurance_categories) {
    # Filter by insurance category
    EL_sum_filtered <- EL_sum %>% filter(insurance_category == ins)

    # Create plot
    plotEL_sy <- ggplot(data = EL_sum_filtered, aes(x = shock_year, group = factor(shock_year), y = SumEL75, fill = SumEL75)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(
        colors = color_gradient,
        limits = c(min(EL_sum_filtered$SumEL75), max(EL_sum_filtered$SumEL75)),
        breaks = c(min(EL_sum_filtered$SumEL75), 0, max(EL_sum_filtered$SumEL75)),
        labels = scales::comma,
        name = "Sum of EL"
      ) +
      facet_grid(insurance_category ~ sector ) +
      theme_2dii() +
      labs(x = "Shock Year", y =  "Sum of EL", title = paste("Sum of EL per Insurance and Sector - 75% LGD - Shock Year -", scenario_name_for_title, "-", ins)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 28),
            axis.text = element_text(size = 27),
            axis.title.x = element_text(size = 26),
            axis.title.y = element_text(size = 26),
            strip.text = element_text(size = 26),
            panel.grid = element_blank(),
            panel.grid.major = element_line(color = "lightgray", size = 0.2),
            legend.text = element_text(size = 22),
            legend.title = element_text(size = 22)) +
      scale_y_continuous(
        labels = scales::label_number_si(scale = 1e-9, suffix = " bn")
      ) +
      scale_x_continuous(breaks = c(2026, 2030, 2034))

    # Output Path and filename
    output_path <- "/Users/2diigermany/Desktop/Work/CDI/new plots_last/SY/EL each insurance/"
    filename <- paste0(output_path, "EL_Shock_year_", ins, "_", scenario_name, ".png")

    # Save the plot
    ggsave(filename, plot = plotEL_sy, width = width, height = height, dpi = dpi)
  }
}
########################################################################
########################################################################
########################################################################
########################################################################
#####Discount Rate Plots

# List of scenario dataframes
Scenarios_Dr <- list(WEO_DR = WEO_DR, WEO_NA_DR = WEO_NA_DR, GCAM_DR = GCAM_DR, REMIND_DR = REMIND_DR, GCAMTax_DR = GCAMTax_DR)


################################## MEAN PD DIFF SY PLOT
###  PD Diff plot Discount Rate
for (scenario_name in names(Scenarios_Dr)) {
  scenario_data <- Scenarios_Dr[[scenario_name]]

  # Merge data
  Discount_rate_bond_long <- merge(bond_data, scenario_data, by=c("term", "id"))

  # Calculate mean pd difference
  mean_dr <- Discount_rate_bond_long %>%
    group_by(insurance_category, sector, discount_rate) %>%
    summarise(mean_pd_difference = mean(pd_difference))

  # Reorder levels of insurance_category based on desired order
  mean_dr$insurance_category <- factor(mean_dr$insurance_category, levels = desired_order)

  # Remove "DR" from scenario name for plot title
  scenario_name_for_title <- gsub("_DR", "", scenario_name)

  # Create plot
  plotsy <- ggplot(data = mean_dr, aes(x = discount_rate, group = factor(discount_rate), y = mean_pd_difference, fill = mean_pd_difference)) +
    geom_bar(stat = "identity") +
    scale_fill_gradientn(colors = color_gradient,
                         limits = c(min(mean_dr$mean_pd_difference), max(mean_dr$mean_pd_difference)),
                         breaks = c(min(mean_dr$mean_pd_difference), 0, max(mean_dr$mean_pd_difference)),
                         labels = scales::comma,
                         name = "Mean pd_difference") +
    facet_grid(insurance_category ~ sector, scales = "free_y") +  # Allow separate scales for y-axis
    theme_2dii() +
    labs(x = "Discount Rate", y = "Mean pd_difference", title = paste("Mean PD Difference by Discount Rate -", scenario_name_for_title)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24),  # Adjust title size
          axis.text = element_text(size = 23),  # Adjust tick label size
          axis.title.x = element_text(size = 26),  # Adjust x-axis label size
          axis.title.y = element_text(size = 26),  # Adjust y-axis label size
          strip.text = element_text(size = 24),
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.2),
          legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20)) +  # Adjust legend title size
    scale_x_continuous(breaks = c(0.04, 0.07, 0.1))

  # Output Path and filename
  output_path <- "/Users/2diigermany/Desktop/Work/CDI/new plots_last/DR/PD DIFF/"
  filename <- paste0(output_path, "PDdiff_Discount_Rate_", scenario_name, ".png")

  # Save the plot
  ggsave(filename, plot = plotsy, width = width, height = height, dpi = dpi)
}
###
###
###  EL plot Discount Rate
Scenarios_Dr <- list(WEO_DR = WEO_DR, WEO_NA_DR = WEO_NA_DR, GCAM_DR = GCAM_DR, REMIND_DR = REMIND_DR, GCAMTax_DR = GCAMTax_DR)

scenarios_sector_weights_DR <- list(
  WEO_DR = sector_weights_WEO,
  WEO_NA_DR = sector_weights_WEONA,
  GCAM_DR = sector_weights_GCAM,
  REMIND_DR = sector_weights_REMIND,
  GCAMTax_DR = sector_weights_GCAMTax
)

for (scenario_name in names(scenarios_sector_weights_DR)) {
  sector_weights_df <- scenarios_sector_weights_DR[[scenario_name]]

  # Identify common columns for merging
  common_columns <- intersect(c("id", "company_name", "sector"), colnames(sector_weights_df))

  # Merge data
  Discount_Rate_bond_long <- merge(bond_data, Scenarios_Dr[[scenario_name]], by = c("term", "id"))
  Discount_Rate_bond_long <- merge(Discount_Rate_bond_long, sector_weights_df, by = common_columns)

  # Calculate EL with 75% LGD
  Discount_Rate_bond_long$EL75 <- Discount_Rate_bond_long$pd_difference * Discount_Rate_bond_long$value_usd * Discount_Rate_bond_long$sector_weight * 0.75

  # Sum over sector and portfolio
  EL_sum <- Discount_Rate_bond_long %>%
    group_by(insurance_category, sector, discount_rate) %>%
    summarise(SumEL75 = sum(EL75))

  # Remove "_DR" from scenario name for plot title
  scenario_name_for_title <- gsub("_DR", "", scenario_name)

  # Reorder levels of insurance_category based on desired order
  desired_order <- c("fraternal", "life", "health", "p_n_c", "meta")
  EL_sum$insurance_category <- factor(EL_sum$insurance_category, levels = desired_order)

  # Create plot
  plotEL_dr <- ggplot(data = EL_sum, aes(x = discount_rate, group = factor(discount_rate), y = SumEL75, fill = SumEL75)) +
    geom_bar(stat = "identity") +
    scale_fill_gradientn(
      colors = color_gradient,
      limits = c(min(EL_sum$SumEL75), max(EL_sum$SumEL75)),
      breaks = c(min(EL_sum$SumEL75), 0, max(EL_sum$SumEL75)),
      labels = scales::comma,
      name = "Sum of EL"
    ) +
    facet_grid(insurance_category ~ sector ) +
    theme_2dii() +
    labs(x = "Discount Rate", y =  "Sum of EL", title = paste("Sum of EL per Insurance and Sector - 75% LGD - Discount Rate -", scenario_name_for_title)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
          axis.text = element_text(size = 23),
          axis.title.x = element_text(size = 26),
          axis.title.y = element_text(size = 26),
          strip.text = element_text(size = 24),
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.2),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)) +
    scale_y_continuous(
      labels = scales::label_number_si(scale = 1e-9, suffix = " bn")
    ) +
    scale_x_continuous(breaks = c(0.04, 0.07, 0.1))

  # Output Path and filename
  output_path <- "/Users/2diigermany/Desktop/Work/CDI/new plots_last/DR/ELtotal/"
  filename <- paste0(output_path, "EL_Discount_Rate_", scenario_name, ".png")

  # Save the plot
  ggsave(filename, plot = plotEL_dr, width = width, height = height, dpi = dpi)
}

#####DR EL for each portfolio

for (scenario_name in names(scenarios_sector_weights_DR)) {
  sector_weights_df <- scenarios_sector_weights_DR[[scenario_name]]

  # Identify common columns for merging
  common_columns <- intersect(c("id", "company_name", "sector"), colnames(sector_weights_df))

  # Merge data
  Discount_Rate_bond_long <- merge(bond_data, Scenarios_Dr[[scenario_name]], by = c("term", "id"))
  Discount_Rate_bond_long <- merge(Discount_Rate_bond_long, sector_weights_df, by = common_columns)

  # Calculate EL with 75% LGD
  Discount_Rate_bond_long$EL75 <- Discount_Rate_bond_long$pd_difference * Discount_Rate_bond_long$value_usd * Discount_Rate_bond_long$sector_weight * 0.75

  # Sum over sector and portfolio
  EL_sum <- Discount_Rate_bond_long %>%
    group_by(insurance_category, sector, discount_rate) %>%
    summarise(SumEL75 = sum(EL75))

  # Remove "_DR" from scenario name for plot title
  scenario_name_for_title <- gsub("_DR", "", scenario_name)

  for (ins in insurance_categories) {
    # Filter by insurance category
    EL_sum_filtered <- EL_sum %>% filter(insurance_category == ins)

    # Create plot
    plotEL_dr <- ggplot(data = EL_sum_filtered, aes(x = discount_rate, group = factor(discount_rate), y = SumEL75, fill = SumEL75)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(
        colors = color_gradient,
        limits = c(min(EL_sum_filtered$SumEL75), max(EL_sum_filtered$SumEL75)),
        breaks = c(min(EL_sum_filtered$SumEL75), 0, max(EL_sum_filtered$SumEL75)),
        labels = scales::comma,
        name = "Sum of EL"
      ) +
      facet_grid(insurance_category ~ sector ) +
      theme_2dii() +
      labs(x = "Discount Rate", y =  "Sum of EL", title = paste("Sum of EL per Sector - 75% LGD - Discount Rate -", scenario_name_for_title, "-", ins)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
            axis.text = element_text(size = 23),
            axis.title.x = element_text(size = 26),
            axis.title.y = element_text(size = 26),
            strip.text = element_text(size = 24),
            panel.grid = element_blank(),
            panel.grid.major = element_line(color = "lightgray", size = 0.2),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20)) +
      scale_y_continuous(
        labels = scales::label_number_si(scale = 1e-9, suffix = " bn")
      ) +
      scale_x_continuous(breaks = c(0.04, 0.07, 0.1))

    # Output Path and filename
    output_path <- "/Users/2diigermany/Desktop/Work/CDI/new plots_last/DR/EL each insurance/"
    filename <- paste0(output_path, "EL_Discount_Rate_", ins, "_", scenario_name_for_title, ".png")

    # Save the plot
    ggsave(filename, plot = plotEL_dr, width = width, height = height, dpi = dpi)
  }
}

######################################################################
######################################################################
### Distribution Plots
### Shock Year

## not looped over scenarios

distribution_data_total <- REMIND_SY

distribution_data <- distribution_data_total %>% select(company_name, sector, business_unit, term, discount_rate, shock_year,  pd_difference, id)

## Merging with bond data
distribution_data_combined <- merge(bond_data, distribution_data, by=c("term", "id"))

# Chart settings
theme_set(theme_bw())
options(scipen = 999)

####. Loop over sectors plot

# Iterate over the desired parameters
shock_years <- c(2026, 2030, 2034)
colors <- c('red', 'magenta', 'blue')

# Get unique insurance categories
unique_categories <- unique(distribution_data_combined$insurance_category)

# Define the output path
output_path <- "/Users/2diigermany/Desktop/Work/CDI/new plots_last/Distribution SY/"

# Iterate over each insurance category
for (insurance_category in unique_categories) {
  # Filter the Shock_data based on the insurance_category column
  filtered_data <- distribution_data_combined[distribution_data_combined$insurance_category == insurance_category, ]

  # Initialize an empty list to store plots
  plots <- list()

  # Create plots for each sector
  for (sector in c("Automotive", "Coal", "Oil&Gas", "Power")) {
    # Create an empty data frame to store computed density values
    density_data <- data.frame()

    # Iterate over each desired parameter and compute density values
    for (i in 1:length(shock_years)) {
      pm <- shock_years[i]
      data <- filtered_data[filtered_data$sector %in% sector & filtered_data$shock_year == pm, ]
      label <- paste("Shock Year", pm)

      density_values <- density(data$pd_difference)
      density_df <- data.frame(x = density_values$x, y = density_values$y, label = label)

      density_data <- rbind(density_data, density_df)
    }

    # Create the plot with lines and different colors
    density_plot <- ggplot(density_data, aes(x = x, y = y, color = label)) +
      geom_line(size = 1) +
      scale_color_manual(values = colors) +
      labs(x = "PD Difference", y = "Density") +
      ggtitle(paste(insurance_category, "-", sector)) +
      scale_x_continuous(labels = percent_format()) +
      theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 13),
            axis.title.x = element_text(size = 13),
            axis.title.y = element_text(size = 13),
            legend.title = element_blank(),
            legend.text = element_text(size = 10),
            legend.position = "top",
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "lightgray", size=0.2),
            panel.border = element_rect(color = "black", fill = NA, size = 0.3),
            legend.background = element_rect(fill = "white"),
            legend.key = element_rect(fill = "white", color = "transparent")
            #panel.grid.minor = element_line(color = "lightgray")
      )

    plots[[sector]] <- density_plot
  }

  # Combine plots into a grid
  combined_plot <- cowplot::plot_grid(plotlist = plots, nrow = 2, ncol = 2)

  # Add a title to the combined plot
  # Change here to the relevant scenario
  title <- ggdraw() + draw_label(paste(insurance_category, "- Distribution of PD Difference - Shock Year - REMIND Global"), size = 18)
  combined_plot <- cowplot::plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1))

  # Modify the plot title element to have a white background
  combined_plot <- combined_plot +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "white"))

  # Save the plot ## change here the scenario in the filename
  plot_filename <- paste0(output_path, insurance_category, "_Distribution_ShockYear_REMIND_Global.png")
  ggsave(plot_filename, combined_plot, width = 10, height = 6)
}

########################
####Discount Rate Plots


#### Select Scenario (only with DR)
distribution_data_total <- REMIND_DR

distribution_data <- distribution_data_total %>% select(company_name, sector, business_unit, term, discount_rate, shock_year,  pd_difference, id)

## Merging with bond data
distribution_data_combined <- merge(bond_data, distribution_data, by=c("term", "id"))

# Chart settings
theme_set(theme_bw())
options(scipen = 999)

####. Loop over sectors plot

# Iterate over the desired parameters
discount_rates <- c(0.04, 0.07, 0.1)
colors <- c('red', 'magenta', 'blue')

# Get unique insurance categories
unique_categories <- unique(distribution_data_combined$insurance_category)

# Define the output path
output_path <- "/Users/2diigermany/Desktop/Work/CDI/new plots_last/DR/Distribution/"

# Iterate over each insurance category
for (insurance_category in unique_categories) {
  # Filter the Shock_data based on the insurance_category column
  filtered_data <- distribution_data_combined[distribution_data_combined$insurance_category == insurance_category, ]

  # Initialize an empty list to store plots
  plots <- list()

  # Create plots for each sector
  for (sector in c("Automotive", "Coal", "Oil&Gas", "Power")) {
    # Create an empty data frame to store computed density values
    density_data <- data.frame()

    # Iterate over each desired parameter and compute density values
    for (i in 1:length(discount_rates)) {
      pm <- discount_rates[i]
      data <- filtered_data[filtered_data$sector %in% sector & filtered_data$discount_rate == pm, ]
      label <- paste("Discount Rate", pm)

      density_values <- density(data$pd_difference)
      density_df <- data.frame(x = density_values$x, y = density_values$y, label = label)

      density_data <- rbind(density_data, density_df)
    }

    # Create the plot with lines and different colors
    density_plot <- ggplot(density_data, aes(x = x, y = y, color = label)) +
      geom_line(size = 1) +
      scale_color_manual(values = colors) +
      labs(x = "PD Difference", y = "Density") +
      ggtitle(paste(insurance_category, "-", sector)) +
      scale_x_continuous(labels = percent_format()) +
      theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 13),
            axis.title.y = element_text(size = 13),
            legend.title = element_blank(),
            legend.text = element_text(size = 13),
            legend.position = "top",
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "lightgray", size=0.2),
            panel.border = element_rect(color = "black", fill = NA, size = 0.3),
            legend.background = element_rect(fill = "white"),
            legend.key = element_rect(fill = "white", color = "transparent")
            #panel.grid.minor = element_line(color = "lightgray")
      )

    plots[[sector]] <- density_plot
  }

  # Combine plots into a grid
  combined_plot <- cowplot::plot_grid(plotlist = plots, nrow = 2, ncol = 2)

  # Add a title to the combined plot
  # change title to reflect scenario
  title <- ggdraw() + draw_label(paste(insurance_category, "- Distribution of PD Difference - Discount Rate - REMIND Global"), size = 18)
  combined_plot <- cowplot::plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1))
  # Modify the plot title element to have a white background
  combined_plot <- combined_plot +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "white"))

  # Save the plot and change the filename to reflect the scenario
  plot_filename <- paste0(output_path, insurance_category, "_Distribution_Discountrate_REMIND_Global.png")
  ggsave(plot_filename, combined_plot, width = 13, height = 6)
}

########################################################################
# additional Plot: Mean PD Diff over all scenarios for Meta
# This Plot is not properly looped and a bit harder to replicate
###additional PD DIFF plots
##
## Merging with cpr bond data

##------needs to be looped so it created mean_shockyear_meta_ for each scenario
##!! Choose Scenario
Shock_Year_bond_long <- merge(bond_data, REMIND_SY, by=c("term", "id")) ###change here data

## Merging with company sector weight data
##!! Choose Weights
Shock_Year_bond_long <- merge(Shock_Year_bond_long,sector_weights_REMIND, by=c("id", "company_name", "sector")) ##change here the merge with sector weights

## calculating unweighted mean over time
mean_shockyear <- Shock_Year_bond_long %>%
  group_by(insurance_category, sector, shock_year) %>% ###change here for parameter
  summarise(mean_pd_difference = mean(pd_difference))

###sampling here the means for each scenario
mean_shockyear_meta_REM<- mean_shockyear[mean_shockyear$insurance_category=="meta",]
###----
###aggregating all into one
##note that you need to rerun the above lines for each scenario to create this plot

mean_shockyear_meta_GCAM$scenario = "GCAM"
mean_shockyear_meta_REM$scenario = "REMIND"
mean_shockyear_meta_WEONA$scenario = "WEO NA"
mean_shockyear_meta_WEO$scenario = "WEO"
mean_shockyear_meta_GCAM_tax$scenario = "GCAM Tax"

## binding
data<- rbind(mean_shockyear_meta_GCAM,mean_shockyear_meta_REM,mean_shockyear_meta_WEO,mean_shockyear_meta_WEONA, mean_shockyear_meta_GCAM_tax)

color_gradient <- c("#FFC0C0", "#FF8888", "#FF6666", "#FF4444", "#FF2222", "#FF0000")

# Define the order of scenarios
scenario_order <- c("WEO", "WEO NA","REMIND","GCAM", "GCAM Tax")

# Reorder the levels of the scenario variable
data$scenario <- factor(data$scenario, levels = scenario_order)
plotsy <- ggplot(data = data, aes(x = shock_year, group = factor(shock_year), y = mean_pd_difference, fill = mean_pd_difference)) + ##change here discount rate
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colors = color_gradient,
                       limits = c(min(data$mean_pd_difference), max(data$mean_pd_difference)),
                       breaks = c(min(data$mean_pd_difference), 0, max(data$mean_pd_difference)),
                       labels = scales::comma,
                       name = "Mean pd_difference") +
  facet_grid(scenario ~ sector )+
  theme_2dii()+
  labs(x = "Shock_Year", y = "Mean pd_difference", title = "Meta: Mean PD Difference by Shock Year and Scenario")+ ###change here
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size= 28),
        axis.text = element_text(size = 24),  # Adjust the font size of axis text
        axis.title.x = element_text(size = 26),  # Adjust the font size of x-axis label
        axis.title.y = element_text(size = 26),
        strip.text = element_text(size = 22),
        panel.grid = element_blank(),  # Remove default gridlines
        panel.grid.major = element_line(color = "lightgray", size=0.2),
        legend.text = element_text(size = 22)# Add major gridlines
        #panel.grid.minor = element_blank()  # Remove minor gridlines
  )+
  scale_x_continuous(breaks = c(2026, 2030, 2034))+
  scale_y_continuous(breaks = c(0.2, 0.6))


output_path <- "/Users/2diigermany/Desktop/Work/CDI/new plots_last/SY/scenario plot/"

filename <- paste0(output_path, "META_PDDiff_allscenarios.png")

# Save the plot with the specified dimensions and dpi
ggsave(filename, plot = plotsy, width = width, height = height, dpi = dpi)


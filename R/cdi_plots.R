library(dplyr)
library(ggplot2)

make_mean_pd_diff_sy_plots <- function(Scenarios_Sy, bond_data){

  MEAN_PD_DIFF_SY_PLOTS <- list()
  for (scenario_name in unique(Scenarios_Sy$scenario_duo)) {
    scenario_data <- Scenarios_Sy %>%
      dplyr::filter(scenario_duo == scenario_name)

    # Merge data
    Shock_Year_bond_long <- dplyr::inner_join(bond_data, scenario_data, by=c("term", "company_id"))

    # Calculate mean pd difference
    mean_shockyear <- Shock_Year_bond_long %>%
      group_by(sector, shock_year) %>% # TODO group by asset_type ?
      summarise(mean_pd_difference = mean(pd_difference))

    # Remove "SY" from scenario name for plot title
    scenario_name_for_title <- scenario_name

    # Create plot
    plotsy <-
      ggplot(data = mean_shockyear, aes(x = factor(shock_year), y = mean_pd_difference, fill = mean_pd_difference)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(colors = color_gradient,
                           limits = c(min(mean_shockyear$mean_pd_difference), max(mean_shockyear$mean_pd_difference)),
                           breaks = c(min(mean_shockyear$mean_pd_difference), 0, max(mean_shockyear$mean_pd_difference)),
                           labels = scales::comma,
                           name = "Mean pd_difference") +
      facet_grid(. ~ sector, scales = "free_y") +  # Allow separate scales for y-axis
      theme_2dii() +
      labs(x = "shock_year", y = "Mean pd_difference", title = paste("Mean PD Difference by Shock Year -", scenario_name_for_title))

    MEAN_PD_DIFF_SY_PLOTS[scenario_name] <- plotsy

  }
}

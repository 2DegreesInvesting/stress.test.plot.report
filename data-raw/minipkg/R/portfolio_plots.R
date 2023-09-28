

#' Title
#'
#' @param Scenarios_Sy
#' @param bond_data
#' @param groups_var
#' @param color_gradient
#'
#' @return
#' @export
#'
#' @examples
make_mean_pd_diff_sy_plots <-
  function(Scenarios_Sy,
           bond_data,
           groups_var,
           color_gradient) {
    MEAN_PD_DIFF_SY_PLOTS <- list()
    for (scenario_name in unique(Scenarios_Sy$scenario_duo)) {
      scenario_data <- Scenarios_Sy %>%
        dplyr::filter(scenario_duo == scenario_name)

      # Merge data
      Shock_Year_bond_long <-
        dplyr::inner_join(
          bond_data,
          scenario_data,
          by = c("term", "company_id"),
          relationship = "many-to-many"
        )

      # Calculate mean pd difference
      mean_shockyear <- Shock_Year_bond_long %>%
        group_by(sector,!!rlang::sym(groups_var)) %>% # TODO group by asset_type ?
        summarise(mean_pd_difference = mean(pd_difference),
                  .groups = "drop")

      # Remove "SY" from scenario name for plot title
      scenario_name_for_title <- scenario_name

      # Create plot
      plotsy <-
        ggplot(data = mean_shockyear,
               aes(
                 x = factor(!!rlang::sym(groups_var)),
                 y = mean_pd_difference,
                 fill = mean_pd_difference
               )) +
        geom_bar(stat = "identity") +
        scale_fill_gradientn(
          colors = color_gradient,
          limits = c(
            min(mean_shockyear$mean_pd_difference),
            max(mean_shockyear$mean_pd_difference)
          ),
          breaks = c(
            min(mean_shockyear$mean_pd_difference),
            0,
            max(mean_shockyear$mean_pd_difference)
          ),
          labels = scales::comma,
          name = "Mean pd_difference"
        ) +
        theme(
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.2)
        ) +
        facet_grid(. ~ sector, scales = "free_y") +  # Allow separate scales for y-axis
        theme_2dii() +
        labs(
          x = groups_var,
          y = "Mean pd_difference",
          title = paste(
            "Mean PD Difference by Shock Year \n",
            scenario_name_for_title
          )
        ) +
        theme(
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.2)
        )

      MEAN_PD_DIFF_SY_PLOTS[[scenario_name]] <- plotsy

    }
    return(MEAN_PD_DIFF_SY_PLOTS)
  }


#' Title
#'
#' @param Scenarios_Sy
#' @param bond_data
#' @param scenario_weights
#' @param groups_var
#' @param color_gradient
#'
#' @return
#' @export
#'
#' @examples
make_expected_loss_plot <-
  function(Scenarios_Sy,
           bond_data,
           scenario_weights,
           groups_var,
           color_gradient) {
    ## EL PLOT LOOP SY
    # Loop through scenarios and corresponding sector weights
    EXPECTED_LOSS_PLOTS <- list()
    for (scenario_name in unique(scenario_weights$scenario_duo)) {
      sector_weights_df <-
        scenario_weights %>% filter(scenario_duo == scenario_name)

      # Merge data
      Shock_Year_bond_long <-
        inner_join(
          bond_data,
          Scenarios_Sy %>% filter(scenario_duo == scenario_name),
          by = c("term", "company_id"),
          relationship = "many-to-many"
        )
      Shock_Year_bond_long <-
        inner_join(
          Shock_Year_bond_long,
          sector_weights_df,
          by = c("company_id", "sector", "scenario_duo")
        )

      # Calculate EL with 75% LGD
      Shock_Year_bond_long$EL75 <-
        Shock_Year_bond_long$pd_difference * Shock_Year_bond_long$value_usd * Shock_Year_bond_long$sector_weight * 0.75

      # Sum over sector and portfolio
      EL_sum <- Shock_Year_bond_long %>%
        group_by(sector, !!rlang::sym(groups_var)) %>%
        summarise(SumEL75 = sum(EL75), .groups = "drop")

      # Remove "_SY" from scenario name for plot title
      scenario_name_for_title <- scenario_name

      # Create plot
      plotEL_sy <-
        ggplot(data = EL_sum,
               aes(
                 x = factor(!!rlang::sym(groups_var)),
                 y = SumEL75,
                 fill = SumEL75
               )) +
        geom_bar(stat = "identity") +
        scale_fill_gradientn(
          colors = color_gradient,
          limits = c(min(EL_sum$SumEL75), max(EL_sum$SumEL75)),
          breaks = c(min(EL_sum$SumEL75), 0, max(EL_sum$SumEL75)),
          labels = scales::comma,
          name = "Sum of EL"
        ) +
        facet_grid(. ~ sector, scales = "free_y") +
        theme_2dii() +
        labs(
          x = groups_var,
          y =  "Sum of EL",
          title = paste(
            "Sum of EL per Insurance and Sector - 75% LGD \n",
            scenario_name_for_title
          )
        ) +
        theme(
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.2)
        )

      EXPECTED_LOSS_PLOTS[[scenario_name]] <- plotEL_sy

    }
    return(EXPECTED_LOSS_PLOTS)
  }


make_discount_rate_plot  <-
  function(Scenarios_Sy,
           bond_data,
           scenario_weights,
           groups_var,
           color_gradient) {
    # EL for each portfolio loop
    #Loop through scenarios and corresponding sector weights
    DISCOUNT_RATE_PLOTS <- list()
    for (scenario_name in unique(scenario_weights$scenario_duo)) {
      sector_weights_df <-
        scenario_weights %>% filter(scenario_duo == scenario_name)

      # Merge data
      Shock_Year_bond_long <-
        merge(
          bond_data,
          Scenarios_Sy %>% filter(scenario_duo == scenario_name),
          by = c("term", "company_id")
        )
      Shock_Year_bond_long <-
        merge(Shock_Year_bond_long, sector_weights_df)

      # Calculate EL with 75% LGD
      Shock_Year_bond_long$EL75 <-
        Shock_Year_bond_long$pd_difference * Shock_Year_bond_long$value_usd * Shock_Year_bond_long$sector_weight * 0.75

      # Sum over sector and portfolio
      EL_sum <- Shock_Year_bond_long %>%
        group_by(sector, !!rlang::sym(groups_var)) %>%
        summarise(SumEL75 = sum(EL75), .groups="drop")

      # Remove "_SY" from scenario name for plot title
      scenario_name_for_title <- scenario_name

      # Create plot
      plotEL_sy <-
        ggplot(data = EL_sum,
               aes(
                 x = factor(!!rlang::sym(groups_var)),
                 y = SumEL75,
                 fill = SumEL75
               )) +
        geom_bar(stat = "identity") +
        scale_fill_gradientn(
          colors = color_gradient,
          limits = c(
            min(EL_sum$SumEL75),
            max(EL_sum$SumEL75)
          ),
          breaks = c(
            min(EL_sum$SumEL75),
            0,
            max(EL_sum$SumEL75)
          ),
          labels = scales::comma,
          name = "Sum of EL"
        ) +
        facet_grid(. ~ sector) +
        theme_2dii() +
        labs(
          x = groups_var,
          y =  "Sum of EL",
          title = paste(
            "Sum of EL per Insurance and Sector - 75% LGD - \n",
            scenario_name_for_title
          )
        ) +
        theme(
          plot.title = element_text(
            hjust = 0.5,
            face = "bold",
            size = 28
          ),
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.2),
        )
      DISCOUNT_RATE_PLOTS[[scenario_name]] <- plotEL_sy
    }

    return(DISCOUNT_RATE_PLOTS)

  }

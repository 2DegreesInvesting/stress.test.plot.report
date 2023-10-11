# remove scientific notation in plots axis
options(scipen = 999)

#' Title
#'
#' @param Scenarios_Sy
#' @param loans_data
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
           loans_data,
           scenario_weights,
           groups_var,
           color_gradient) {
    ## EL PLOT LOOP SY
    # Loop through scenarios and corresponding sector weights
    EXPECTED_LOSS_PLOTS <- list()
    for (scenario_name in unique(Scenarios_Sy$scenario_duo)) {
      plist <- list()
      plot_i <- 1
      for (pd_type in c("pd_baseline", "pd_shock", "pd_difference")) {
        # Merge data
        Shock_Year_bond_long <-
          inner_join(
            loans_data,
            Scenarios_Sy %>% filter(scenario_duo == scenario_name),
            by = c("term", "company_name", "sector"),
            relationship = "many-to-many"
          )
        Shock_Year_bond_long <-
          inner_join(
            Shock_Year_bond_long,
            scenario_weights,
            by = c("bank", "company_name", "sector")
          )

        # Calculate EL
        Shock_Year_bond_long$EL75 <-
          Shock_Year_bond_long[[pd_type]] * Shock_Year_bond_long$exposure_value * Shock_Year_bond_long$weight * Shock_Year_bond_long$lgd

        # Sum over sector and portfolio
        EL_sum <- Shock_Year_bond_long %>%
          group_by(bank, sector,!!rlang::sym(groups_var)) %>%
          summarise(SumEL75 = sum(EL75), .groups = "drop")

        # Scale EL to million
        EL_sum$SumEL75 <- EL_sum$SumEL75 / 1e6

        # generate plot title used as subtitle
        plot_title <- paste("With", pd_type)

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
            colors = color_gradient[[pd_type]],
            limits = c(min(EL_sum$SumEL75), max(EL_sum$SumEL75)),
            breaks = c(min(EL_sum$SumEL75), 0, max(EL_sum$SumEL75)),
            labels = scales::comma,
            name = "Sum of EL"
          ) +
          facet_grid(bank ~ sector, scales = "free_y") +
          theme_2dii() +
          labs(
            x = groups_var,
            y =  "Sum of EL (in millions)",
            title=plot_title
          ) +
          theme(
            panel.grid = element_blank(),
            panel.grid.major = element_line(color = "lightgray", size = 0.2),
            plot.title=element_text(size = 12, face = "bold")
          )

        plist[[plot_i]] <- plotEL_sy
        plot_i <- plot_i + 1

      }
      top_title <- ggpubr::text_grob(paste0("Expected Loss per portfolio and sector\n",
                                    scenario_name),
                             size = 15, face = "bold")
      plots_grid <-
        do.call("grid.arrange",
                list(
                  grobs=plist,
                  nrow = length(plist),
                  top =top_title
                ))
      EXPECTED_LOSS_PLOTS[[scenario_name]] <- plots_grid

    }
    return(EXPECTED_LOSS_PLOTS)
  }


#' Title
#'
#' @param Scenarios_Sy
#' @param loans_data
#' @param groups_var
#' @param color_gradient
#'
#' @return
#' @export
#'
#' @examples
make_average_pd_shock_plots <-
  function(Scenarios_Sy,
           loans_data,
           scenario_weights,
           groups_var,
           color_gradient) {
    MEAN_PD_DIFF_SY_PLOTS <- list()
    for (scenario_name in unique(Scenarios_Sy$scenario_duo)) {
      plist <- list()
      plot_i <- 1
      for (pd_type in c("pd_baseline", "pd_shock", "pd_difference")) {
      scenario_data <- Scenarios_Sy %>%
        dplyr::filter(scenario_duo == scenario_name)

      # Merge data
      Shock_Year_bond_long <-
        dplyr::inner_join(
          loans_data,
          scenario_data,
          by = c("term", "company_name", "sector"),
          relationship = "many-to-many"
        )
      Shock_Year_bond_long <-
        inner_join(Shock_Year_bond_long,
                   scenario_weights,
                   by = c("bank", "company_name", "sector"))


      # Calculate mean pd difference
      mean_shockyear <- Shock_Year_bond_long %>%
        group_by(bank, sector,!!rlang::sym(groups_var)) %>%
        # TODO duplicate by another one on pd_shock. Both should be weighted
        #   Calculate mean_pd_shock as
        #     (pd_shock * exposure_usd) / sum(exposure_company_usd)
        summarise(mean_pd_shock = mean(.data[[pd_type]] * .data$weight),
                  .groups = "drop")

      # generate plot title used as subtitle
      plot_title <- paste("With", pd_type)

      # Create plot
      plotsy <-
        ggplot(data = mean_shockyear,
               aes(
                 x = factor(!!rlang::sym(groups_var)),
                 y = mean_pd_shock,
                 fill = mean_pd_shock
               )) +
        geom_bar(stat = "identity") +
        scale_fill_gradientn(
          colors = color_gradient[[pd_type]],
          limits = c(
            min(mean_shockyear$mean_pd_shock),
            max(mean_shockyear$mean_pd_shock)
          ),
          breaks = c(
            min(mean_shockyear$mean_pd_shock),
            0,
            max(mean_shockyear$mean_pd_shock)
          ),
          labels = scales::comma,
          name = "Mean PD Shock"
        ) +
        theme(
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.2)
        ) +
        facet_grid(bank ~ sector, scales = "free_y") +  # Allow separate scales for y-axis
        theme_2dii() +
        labs(
          x = groups_var,
          y = "Average PD Shock",
          title = plot_title
        ) +
        theme(
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.2),
          plot.title=element_text(size = 12, face = "bold")

        )

      plist[[plot_i]] <- plotsy
      plot_i <- plot_i + 1

      }
      top_title <- ggpubr::text_grob(paste0("Average PD Shock by Portfolio and Sector \n",
                                            scenario_name),
                                     size = 15, face = "bold")
      plots_grid <-
        do.call("grid.arrange",
                list(
                  grobs=plist,
                  nrow = length(plist),
                  top =top_title
                ))
      MEAN_PD_DIFF_SY_PLOTS[[scenario_name]] <- plots_grid

    }
    return(MEAN_PD_DIFF_SY_PLOTS)
  }

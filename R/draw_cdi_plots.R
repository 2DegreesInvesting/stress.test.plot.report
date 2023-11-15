#' Title
#'
#' @param data_cdi_el_plot
#' @param x_var
#' @param y_var
#' @param fill_var
#' @param facet_rows_var
#' @param facet_cols_var
#' @param title
#'
#' @return
#' @export
#'
#' @examples
make_expected_loss_plot <-
  function(data_cdi_el_plot,
           x_var,
           y_var,
           fill_var,
           facet_rows_var,
           facet_cols_var,
           title) {


    # Create plot
    plotEL <-
      ggplot(data = data_cdi_el_plot,
             aes(
               x = factor(!!rlang::sym(x_var)),
               y = !!rlang::sym(y_var),
               fill = !!rlang::sym(fill_var)
             )) +
      geom_bar(stat = "identity") +
      ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
      scale_fill_gradient(
        low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
        high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
      )+
      facet_wrap(
        c(facet_rows_var,facet_cols_var),
        scales = "free_y"
      ) +
      theme_2dii() +
      labs(x = x_var,
           y =  "Sum of EL (in millions)",
           title = title) +
      theme(
        panel.grid = element_blank(),
        panel.grid.major = ggplot2::element_line(color = "lightgray", size = 0.2)
      )

    return(plotEL)

  }

#'
#' #' Title
#' #'
#' #' @param Scenarios_Sy
#' #' @param loans_data
#' #' @param groups_var
#' #' @param color_gradient
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' make_average_pd_shock_plots <-
#'   function(Scenarios_Sy,
#'            loans_data,
#'            scenario_weights,
#'            groups_var,
#'            color_gradient,
#'            pd_types,
#'            run_id_name) {
#'     MEAN_PD_DIFF_SY_PLOTS <- list()
#'     for (run_id_name in unique(Scenarios_Sy$run_id)) {
#'       plist <- list()
#'       plot_i <- 1
#'
#'       plist[[plot_i]] <- plotsy
#'       plot_i <- plot_i + 1
#'
#'       for (pd_type in pd_types) {
#'         scenario_data <- Scenarios_Sy %>%
#'           dplyr::filter(run_id == run_id_name)
#'
#'         # Merge data
#'         Shock_Year_bond_long <-
#'           dplyr::inner_join(
#'             loans_data,
#'             scenario_data,
#'             by = c("term", "company_name", "sector"),
#'             relationship = "many-to-many"
#'           )
#'         Shock_Year_bond_long <-
#'           inner_join(
#'             Shock_Year_bond_long,
#'             scenario_weights,
#'             by = c("bank", "company_name", "sector")
#'           )
#'
#'
#'         # Calculate mean pd difference
#'         mean_shockyear <- Shock_Year_bond_long %>%
#'           group_by(bank, sector, !!rlang::sym(groups_var)) %>%
#'           # TODO duplicate by another one on pd_shock. Both should be weighted
#'           #   Calculate mean_pd_shock as
#'           #     (pd_shock * exposure_usd) / sum(exposure_company_usd)
#'           summarise(mean_pd_shock = mean(.data[[pd_type]] * .data$weight),
#'                     .groups = "drop")
#'
#'         # generate plot title used as subtitle
#'         plot_title <- paste("With", pd_type)
#'
#'         # Create plot
#'         plotsy <-
#'           ggplot(data = mean_shockyear,
#'                  aes(
#'                    x = factor(!!rlang::sym(groups_var)),
#'                    y = mean_pd_shock,
#'                    fill = mean_pd_shock
#'                  )) +
#'           geom_bar(stat = "identity") +
#'           scale_fill_gradientn(
#'             colors = color_gradient[[pd_type]],
#'             limits = c(
#'               min(mean_shockyear$mean_pd_shock),
#'               max(mean_shockyear$mean_pd_shock)
#'             ),
#'             breaks = c(
#'               min(mean_shockyear$mean_pd_shock),
#'               0,
#'               max(mean_shockyear$mean_pd_shock)
#'             ),
#'             labels = scales::comma,
#'             name = "Mean PD Shock"
#'           ) +
#'           theme(
#'             panel.grid = element_blank(),
#'             panel.grid.major = element_line(color = "lightgray", size = 0.2)
#'           ) +
#'           facet_grid(sector ~ bank, scales = "free_y") +  # Allow separate scales for y-axis
#'           theme_2dii() +
#'           labs(x = groups_var,
#'                y = "Average PD Shock",
#'                title = plot_title) +
#'           theme(
#'             panel.grid = element_blank(),
#'             panel.grid.major = element_line(color = "lightgray", size = 0.2),
#'             plot.title = element_text(size = 12, face = "bold")
#'
#'           )
#'
#'         plist[[plot_i]] <- plotsy
#'         plot_i <- plot_i + 1
#'
#'       }
#'       top_title <-
#'         ggpubr::text_grob(
#'           paste0("Average PD Shock by Portfolio and Sector \n",
#'                  run_id_name),
#'           size = 15,
#'           face = "bold"
#'         )
#'       plots_grid <-
#'         do.call("grid.arrange",
#'                 list(
#'                   grobs = plist,
#'                   nrow = length(plist),
#'                   top = top_title
#'                 ))
#'       MEAN_PD_DIFF_SY_PLOTS[[run_id_name]] <- plots_grid
#'
#'     }
#'     return(MEAN_PD_DIFF_SY_PLOTS)
#'   }

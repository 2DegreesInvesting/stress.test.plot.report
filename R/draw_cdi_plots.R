color_gradient <- c("#FFC0C0", "#FF8888", "#FF6666", "#FF4444", "#FF2222", "#FF0000")


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
           facet_var,
           title) {
    # Create plot
    plotEL <-
      ggplot(
        data = data_cdi_el_plot,
        aes(
          x = factor(!!rlang::sym(x_var)),
          y = !!rlang::sym(y_var),
          fill = !!rlang::sym(fill_var)
        )
      ) +
      geom_bar(stat = "identity") +
      theme_2dii() +
      ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
      scale_fill_gradient(
        low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
        high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
        labels = scales::unit_format(unit = "M", scale = 1e-6)
      ) +
      facet_wrap(
        facet_var,
        scales = "free_y"
      ) +
      labs(
        x = x_var,
        y = "Sum of EL (in millions)",
        title = title
      ) +
      theme(
        panel.grid = element_blank(),
        panel.grid.major = ggplot2::element_line(color = "lightgray", size = 0.2)
      )

    return(plotEL)
  }


#' Title
#'
#' @param data_cdi_pd_plot
#' @param scenario_name_for_title
#'
#' @return
#' @export
#'
#' @examples
make_mean_pd_diff_plot <- function(data_cdi_pd_plot, scenario_name_for_title) {
  # Create plot
  plotsy <- ggplot(data = data_cdi_pd_plot, aes(x = factor(crispy.shock_year), group = factor(crispy.shock_year), y = pd_difference, fill = pd_difference)) +
    geom_bar(stat = "identity") +
    scale_fill_gradientn(
      colors = color_gradient,
      limits = c(min(data_cdi_pd_plot$pd_difference), max(data_cdi_pd_plot$pd_difference)),
      breaks = c(min(data_cdi_pd_plot$pd_difference), 0, max(data_cdi_pd_plot$pd_difference)),
      labels = scales::comma,
      name = "Mean pd_difference"
    ) +
    facet_grid(~portfolio.ald_sector, scales = "free_y") + # Allow separate scales for y-axis
    theme_2dii() +
    labs(x = "Shock_Year", y = "Mean pd_difference", title = paste("Mean PD Difference by Shock Year -", scenario_name_for_title))
  # theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 28),  # Adjust title size
  #       axis.text = element_text(size = 23),  # Adjust tick label size
  #       axis.title.x = element_text(size = 26),  # Adjust x-axis label size
  #       axis.title.y = element_text(size = 26),  # Adjust y-axis label size
  #       strip.text = element_text(size = 24),
  #       panel.grid = element_blank(),
  #       panel.grid.major = element_line(color = "lightgray", size = 0.2),
  #       legend.text = element_text(size = 20),  # Adjust legend text size
  #       legend.title = element_text(size = 20))

  return(plotsy)
}


#' Title
#'
#' @param plot_data
#' @param scenario_name_for_title
#' @param y_var
#'
#' @return
#' @export
#'
#' @examples
make_discount_rate_plot <- function(plot_data, scenario_name_for_title, y_var) {
  # Create plot
  plotsy <- ggplot(data = plot_data, aes(x = factor(crispy.discount_rate), group = factor(crispy.discount_rate), y = !!rlang::sym(y_var), fill = !!rlang::sym(y_var))) +
    geom_bar(stat = "identity") +
    scale_fill_gradientn(
      colors = color_gradient,
      limits = c(min(plot_data[[y_var]]), max(plot_data[[y_var]])),
      breaks = c(min(plot_data[[y_var]]), 0, max(plot_data[[y_var]])),
      labels = scales::comma,
      name = "Mean pd_difference"
    ) +
    facet_grid(~portfolio.ald_sector, scales = "free_y") + # Allow separate scales for y-axis
    theme_2dii() +
    labs(x = "Discount Rate", y = "Mean pd_difference", title = paste("Mean PD Difference by Discount Rate -", scenario_name_for_title))
  # theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24),  # Adjust title size
  #       axis.text = element_text(size = 23),  # Adjust tick label size
  #       axis.title.x = element_text(size = 26),  # Adjust x-axis label size
  #       axis.title.y = element_text(size = 26),  # Adjust y-axis label size
  #       strip.text = element_text(size = 24),
  #       panel.grid = element_blank(),
  #       panel.grid.major = element_line(color = "lightgray", size = 0.2),
  #       legend.text = element_text(size = 20),  # Adjust legend text size
  #       legend.title = element_text(size = 20)) +  # Adjust legend title size
  # scale_x_continuous(breaks = c(0.04, 0.07, 0.1))
  return(plotsy)
}


#' Title
#'
#' @param data_cdi_pd_plot
#' @param density_var
#'
#' @return
#' @export
#'
#' @examples
make_density_plots <- function(data_cdi_pd_plot, numeric_values, density_var, group_variable = "portfolio.ald_sector") {
  density_var_values <- unique(data_cdi_pd_plot[[density_var]])
  plots <- list()
  # Create plots for each sector
  for (grouper in unique(data_cdi_pd_plot[[group_variable]])) {
    # Create an empty data frame to store computed density values
    density_data <- data.frame()

    # Iterate over each desired parameter and compute density values
    for (i in 1:length(density_var_values)) {
      pm <- density_var_values[i]
      data <- data_cdi_pd_plot %>% dplyr::filter(
        !!rlang::sym(group_variable) == grouper,
        !!rlang::sym(density_var) == pm
      )
      label <- paste(pm)

      density_values <- density(data[[numeric_values]])

      density_values$y <- density_values$y * diff(density_values$x[1:2]) # scale to area under the curve = 1

      density_df <- data.frame(x = density_values$x, y = density_values$y, setNames(list(label), density_var))

      density_data <- rbind(density_data, density_df)
    }

    # Create the plot with lines and different colors
    density_plot <- ggplot(density_data, aes(x = x, y = y, color = !!rlang::sym(density_var))) +
      geom_line(size = 1) +
      # scale_color_manual(values = colors) +
      labs(x = numeric_values, y = "Density") +
      ggtitle(grouper) +
      scale_x_continuous(labels = scales::percent_format()) +
      theme(
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
        axis.title.x = element_blank(), # element_text(size = 13),
        axis.title.y = element_text(size = 13),
        # legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "top",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgray", size = 0.2),
        panel.border = element_rect(color = "black", fill = NA, size = 0.3),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white", color = "transparent")
        # panel.grid.minor = element_line(color = "lightgray")
      )

    plots[[grouper]] <- density_plot
  }



  combined_plot <-
    ggpubr::ggarrange(
      plotlist = plots,
      widths = c(2, 2),
      common.legend = TRUE,
      legend = "right"
    )

  combined_plot <- ggpubr::annotate_figure(
    combined_plot
    # ,top = ggpubr::text_grob(
    #   paste("Distribution of ",numeric_values," - ", group_variable),
    #   face = "bold",
    #   size = 14
    # )
  )

  return(combined_plot)
}

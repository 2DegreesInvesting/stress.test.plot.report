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
      ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
      scale_fill_gradient(
        low = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "grey") %>% pull(.data$hex),
        high = r2dii.colours::palette_1in1000_plot %>% filter(.data$label == "red") %>% pull(.data$hex),
      ) +
      facet_wrap(
        facet_var,
        scales = "fixed"
      ) +
      theme_2dii() +
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
    facet_grid( ~ portfolio.ald_sector, scales = "free_y") + # Allow separate scales for y-axis
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
    facet_grid( ~ portfolio.ald_sector, scales = "free_y") + # Allow separate scales for y-axis
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
make_density_plots <- function(data_cdi_pd_plot, density_var) {
  density_var_values <- unique(data_cdi_pd_plot[[density_var]])
  plots <- list()
  # Create plots for each sector
  for (sector in unique(plot_data$portfolio.ald_sector)) {
    # Create an empty data frame to store computed density values
    density_data <- data.frame()

    # Iterate over each desired parameter and compute density values
    for (i in 1:length(density_var_values)) {
      pm <- density_var_values[i]
      data <- data_cdi_pd_plot %>% dplyr::filter(
        portfolio.ald_sector == sector,
        !!rlang::sym(density_var) == pm
      )
      label <- paste(density_var, pm)

      density_values <- density(data$pd_difference)
      density_df <- data.frame(x = density_values$x, y = density_values$y, label = label)

      density_data <- rbind(density_data, density_df)
    }

    # Create the plot with lines and different colors
    density_plot <- ggplot(density_data, aes(x = x, y = y, color = label)) +
      geom_line(size = 1) +
      # scale_color_manual(values = colors) +
      labs(x = "PD Difference", y = "Density") +
      ggtitle(paste("", "-", sector)) +
      scale_x_continuous(labels = scales::percent_format()) +
      theme(
        plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.title = element_blank(),
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

    plots[[sector]] <- density_plot
  }

  # Combine plots into a grid
  combined_plot <- cowplot::plot_grid(plotlist = plots, nrow = 2, ncol = 2)

  # Add a title to the combined plot
  # Change here to the relevant scenario
  title <- cowplot::ggdraw() + cowplot::draw_label(paste(scenario_name, "- Distribution of PD Difference - ", density_var), size = 18)
  combined_plot <- cowplot::plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1))

  # Modify the plot title element to have a white background
  combined_plot <- combined_plot +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white")
    )

  return(combined_plot)
}

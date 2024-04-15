color_gradient <- c("#FFC0C0", "#FF8888", "#FF6666", "#FF4444", "#FF2222", "#FF0000")


#' Title
#'
#' @param data_cdi_el_plot data_cdi_el_plot
#' @param x_var x_var
#' @param y_var y_var
#' @param fill_var fill_var
#' @param facet_rows_var facet_rows_var
#' @param facet_cols_var facet_cols_var
#' @param title title
#'
#' @export
#'
make_expected_loss_plot <-
  function(data_cdi_el_plot,
           x_var,
           y_var,
           fill_var,
           facet_var,
           title) {
    # Create plot
    plotEL <-
      ggplot2::ggplot(
        data = data_cdi_el_plot,
        ggplot2::aes(
          x = factor(!!rlang::sym(x_var)),
          y = !!rlang::sym(y_var),
          fill = !!rlang::sym(fill_var)
        )
      ) +
      ggplot2::geom_bar(stat = "identity") +
      r2dii.plot::theme_2dii() +
      ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
      ggplot2::scale_fill_gradient(
        low = r2dii.colours::palette_1in1000_plot %>% dplyr::filter(.data$label == "grey") %>% dplyr::pull(.data$hex),
        high = r2dii.colours::palette_1in1000_plot %>% dplyr::filter(.data$label == "red") %>% dplyr::pull(.data$hex),
        labels = scales::unit_format(unit = "M", scale = 1e-6)
      ) +
      ggplot2::facet_wrap(
        facet_var,
        scales = "free_y"
      ) +
      ggplot2::labs(
        x = x_var,
        y = "Sum of EL (in millions)",
        title = title
      ) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "lightgray", size = 0.2)
      )

    return(plotEL)
  }


#' Title
#'
#' @param data_cdi_pd_plot data_cdi_pd_plot
#' @param scenario_name_for_title scenario_name_for_title
#'
#' @export
make_mean_pd_diff_plot <- function(data_cdi_pd_plot, scenario_name_for_title) {
  # Create plot
  plotsy <- ggplot2::ggplot(
    data = data_cdi_pd_plot,
    ggplot2::aes(x = factor(shock_year), group = factor(shock_year), y = pd_difference, fill = pd_difference)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_gradientn(
      colors = color_gradient,
      limits = c(min(data_cdi_pd_plot$pd_difference), max(data_cdi_pd_plot$pd_difference)),
      breaks = c(min(data_cdi_pd_plot$pd_difference), 0, max(data_cdi_pd_plot$pd_difference)),
      labels = scales::comma,
      name = "Mean pd_difference"
    ) +
    ggplot2::facet_grid(~ald_sector, scales = "free_y") + # Allow separate scales for y-axis
    r2dii.plot::theme_2dii() +
    ggplot2::labs(x = "Shock_Year", y = "Mean pd_difference", title = paste("Mean PD Difference by Shock Year -", scenario_name_for_title))
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
#' @param plot_data plot_data
#' @param scenario_name_for_title scenario_name_for_title
#' @param y_var y_var
#'
#' @export
#'
make_discount_rate_plot <- function(plot_data, scenario_name_for_title, y_var) {
  # Create plot
  plotsy <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(x = factor(discount_rate), group = factor(discount_rate), y = !!rlang::sym(y_var), fill = !!rlang::sym(y_var))
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_gradientn(
      colors = color_gradient,
      limits = c(min(plot_data[[y_var]]), max(plot_data[[y_var]])),
      breaks = c(min(plot_data[[y_var]]), 0, max(plot_data[[y_var]])),
      labels = scales::comma,
      name = "Mean pd_difference"
    ) +
    ggplot2::facet_grid(~ald_sector, scales = "free_y") + # Allow separate scales for y-axis
    r2dii.plot::theme_2dii() +
    ggplot2::labs(x = "Discount Rate", y = "Mean pd_difference", title = paste("Mean PD Difference by Discount Rate -", scenario_name_for_title))
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
#' @param data_cdi_pd_plot data_cdi_pd_plot
#' @param density_var density_var
#'
#' @export
#'
make_density_plots <- function(data_cdi_pd_plot, numeric_values, density_var, group_variable = "ald_sector") {
  density_var_values <- unique(data_cdi_pd_plot[[density_var]])
  plots <- list()
  # Create plots for each sector
  for (grouper in unique(data_cdi_pd_plot[[group_variable]])) {
    # Create an empty data frame to store computed density values
    density_data <- data.frame()

    # Iterate over each desired parameter and compute density values
    for (i in seq_along(density_var_values)) {
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
    density_plot <- ggplot2::ggplot(
        density_data,
        ggplot2::aes(x = x, y = y, color = !!rlang::sym(density_var))
        ) +
      ggplot2::geom_line(size = 1) +
      # scale_color_manual(values = colors) +
      ggplot2::labs(x = numeric_values, y = "Density") +
      ggplot2::ggtitle(grouper) +
      ggplot2::scale_x_continuous(labels = scales::percent_format()) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
        axis.text = ggplot2::element_text(size = 13),
        axis.text.x = ggplot2::element_text(size = 10, angle = 45, vjust = 0.5),
        axis.title.x = ggplot2::element_blank(), # element_text(size = 13),
        axis.title.y = ggplot2::element_text(size = 13),
        # legend.title = element_blank(),
        legend.text = ggplot2::element_text(size = 10),
        legend.position = "top",
        plot.background = ggplot2::element_rect(fill = "white"),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major = ggplot2::element_line(color = "lightgray", size = 0.2),
        panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 0.3),
        legend.background = ggplot2::element_rect(fill = "white"),
        legend.key = ggplot2::element_rect(fill = "white", color = "transparent")
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

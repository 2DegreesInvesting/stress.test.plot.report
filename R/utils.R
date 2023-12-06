repeat_plot_in_a_grid <- function(data, plot_fun,  title = "", row_layout = TRUE) {
  plist <- list()
  plot_i <- 1

  for (metric_el_name in metrics_el) {
    plot_title <- paste("With", metric_el_name)

    cdi_plot <- make_expected_loss_plot(
      data_cdi_el_plot = data_cdi_el_plot %>%
        filter(crispy.run_id == run_id_name),
      x_var = "crispy.shock_year",
      y_var = metric_el_name,
      fill_var = metric_el_name,
      facet_rows_var = NULL,
      facet_cols_var = "portfolio.ald_business_unit"
    ) +
      labs(title = plot_title) +
      theme(plot.title = element_text(size = 12, face = "bold"))

    plist[[plot_i]] <- cdi_plot
    plot_i <- plot_i + 1

  }

  top_title <- ggpubr::text_grob(title,
                                 size = 15,
                                 face = "bold")

  if (row_layout == TRUE) {
    plots_grid <- gridExtra::grid.arrange(grobs = plist,
                                          nrow = length(plist),
                                          top = top_title)
  } else{
    plots_grid <- gridExtra::grid.arrange(grobs = plist,
                                          ncol = length(plist),
                                          top = top_title)
  }

  return(plots_grid)

}

qplot_worst_performers <-
  function(data_exposure_plot,
           group_variable_char = "ald_sector",
           performer_entity = "company_name",
           n_performers = 10) {
    data_worst_performers <- data_exposure_plot %>%
      dplyr::group_by_at(c("group_variable", performer_entity)) %>%
      summarise(value_to_plot = sum(.data$value_to_plot)) %>%
      slice_max(order_by = .data$value_to_plot, n = n_performers) %>%
      ungroup() %>%
      mutate(company_name = reorder_within(
        !!rlang::sym(performer_entity),
        .data$value_to_plot,
        .data$group_variable
      ))


    p <- ggplot(
      data_worst_performers,
      aes(
        x = !!rlang::sym(performer_entity),
        y =  .data$value_to_plot,
        fill = .data$group_variable
      )
    ) +
      geom_bar(stat = "identity") +
      geom_text(aes(
        label = scales::unit_format(unit = "M", scale = 1e-6)(.data$value_to_plot),
        hjust = -0.2
      ),
      stat = "identity") +
      scale_x_reordered() +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      scale_fill_2dii("pacta", colour_groups = data_worst_performers$group_variable) +
      coord_flip() +
      theme_2dii() +
      theme(axis.title.y = element_blank(),
            legend.position = "none") +
      facet_wrap(
        ~ fct_reorder(
          .data$group_variable,
          .data$value_to_plot,
          .fun = max,
          .desc = TRUE
        ),
        scales = "free_y",
        ncol = 1
      ) +
      labs(
        title = paste0(
          performer_entity,
          " with the highest exposure within ",
          group_variable_char
        ),
        y = "Exposure"
      )
    p
  }

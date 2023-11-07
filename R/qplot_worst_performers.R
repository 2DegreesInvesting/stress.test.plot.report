qplot_worst_performers <-
  function(data_performers_plot) {

    p <- ggplot(
      data_performers_plot,
      aes(
        x = .data$performer_variable,
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
      scale_y_continuous(expand = expansion(mult = c(0, .1)), label = scales::unit_format(unit = "M", scale = 1e-6)) +
      ggplot2::scale_color_manual(breaks=data_summary_plot$group_variable,
                                  values=r2dii.colours::palette_1in1000_plot[-c(1:2),][c(1:length(unique(data_summary_plot$group_variable))),]$hex )+
      # scale_fill_2dii("pacta", colour_groups = data_worst_performers$group_variable) +
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
        # title = paste0(
        #   performer_entity,
        #   " with the highest exposure within ",
        #   group_variable_char
        # ),
        y = "Exposure"
      )
    p
  }

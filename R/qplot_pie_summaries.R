#' Title
#'
#' @param data_summary_plot data_summary_plot
#' @param agg_sum_name_chr "group_sum" or "group_n"
#'
#' @export
#'
qplot_pie_summaries <-
  function(data_summary_plot, agg_sum_name_chr) {
    data_pie_summary_perc <- data_summary_plot |>
      dplyr::filter(agg_sum_name == agg_sum_name_chr) |>
      dplyr::arrange(.data$group_variable) %>%
      dplyr::mutate(
        csum = rev(cumsum(rev(perc_of_total))),
        pos = (perc_of_total) / 2 + dplyr::lead(csum, 1),
        pos = dplyr::if_else(is.na(pos), (perc_of_total) / 2, pos)
      )

    p1 <-
      ggplot2::ggplot(
        data_pie_summary_perc,
        ggplot2::aes(
          x = "",
          y = .data$perc_of_total,
          fill = .data$group_variable
        )
      ) +
      ggplot2::geom_col(width = 1, color = 1) +
      ggplot2::coord_polar(theta = "y") +
      ggrepel::geom_label_repel(
        ggplot2::aes(
          label = scales::percent_format(accuracy = 0.01)(.data$perc_of_total),
          y = .data$pos
          #   fill = .data$group_variable,
          #   label = scales::unit_format(unit = "M", scale = 1e-6)(.data$agg_sum_value)
        ),
        color = "white",
        segment.color = "black",
        size = 4.5,
        nudge_x = 1,
        show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(
        breaks = data_summary_plot$group_variable,
        values = r2dii.colours::palette_1in1000_plot[-c(1:2), ][c(1:length(unique(data_summary_plot$group_variable))), ]$hex
      ) +
      # r2dii.colours::scale_fill_2dii("2dii", colour_groups = data_summary_plot$group_variable) +
      r2dii.plot::theme_2dii() +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          size = 8,
          hjust = 0,
          face = "italic",
          color = "black"
        )
      )



    data_pie_summary_sum <- data_summary_plot |>
      dplyr::filter(.data$agg_sum_name == agg_sum_name_chr) |>
      dplyr::arrange(.data$group_variable) %>%
      dplyr::mutate(
        csum = rev(cumsum(rev(.data$agg_sum_value))),
        pos = (agg_sum_value) / 2 + dplyr::lead(.data$csum, 1),
        pos = dplyr::if_else(is.na(.data$pos), (.data$agg_sum_value) / 2, .data$pos)
      )



    p2 <-
      ggplot2::ggplot(
        data_pie_summary_sum,
        ggplot2::aes(
          x = "",
          y = .data$agg_sum_value,
          fill = .data$group_variable
        )
      ) +
      ggplot2::geom_col(width = 1, color = 1) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::scale_color_manual(
        breaks = data_summary_plot$group_variable,
        values = r2dii.colours::palette_1in1000_plot[-c(1:2), ][c(1:length(unique(data_summary_plot$group_variable))), ]$hex
      ) +

      # r2dii.colours::scale_fill_2dii("pacta", colour_groups = data_summary_plot$group_variable) +
      r2dii.plot::theme_2dii() +
      ggrepel::geom_label_repel(
        aes(
          y = .data$pos,
          label = scales::unit_format(unit = "M", scale = 1e-6)(.data$agg_sum_value)
        ),
        color = "white",
        segment.color = "black",
        size = 4.5,
        nudge_x = 1,
        show.legend = FALSE
      ) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          size = 8,
          hjust = 0,
          face = "italic",
          color = "black"
        )
      )


    p <-
      ggpubr::ggarrange(
        p1,
        p2,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
      ) +
      ggplot2::labs(title = paste("Summary pie charts", agg_sum_name_chr))
    p <-
      ggpubr::annotate_figure(p,
        top = ggpubr::text_grob(
          paste(
            "Portfolio composition perc of total, counting type as",
            agg_sum_name_chr
          ),
          color = "black",
          face = "bold",
          size = 14
        )
      )
    return(p)
  }

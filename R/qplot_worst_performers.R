#' Title
#'
#' @param data_performers_plot data_performers_plot
#' @param x_var x_var
#' @param y_var y_var
#' @param fill_var fill_var
#' @param facet_var facet_var
#'
#' @export
#'
qplot_worst_performers <-
  function(data_performers_plot, x_var, y_var, fill_var, facet_var) {
    fill_colors <- r2dii.colours::palette_1in1000_plot[-c(1:2), ][c(1:length(unique(data_performers_plot[, fill_var]))), ]$hex

    # reorder_within
    p <- ggplot2::ggplot(
      data_performers_plot,
      ggplot2::aes(
        x = !!rlang::sym(x_var),
        y = !!rlang::sym(y_var),
        fill = !!rlang::sym(fill_var)
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(
        label = scales::unit_format(unit = "M", scale = 1e-6)(.data[[y_var]]),
        hjust = -0.2
      ), stat = "identity") +
      tidytext::scale_x_reordered() +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1)), label = scales::unit_format(unit = "M", scale = 1e-6)) +
      ggplot2::scale_color_manual(
        breaks = data_performers_plot[[facet_var]],
        values = fill_colors
      ) +
      # scale_fill_2dii("pacta", colour_groups = data_worst_performers$group_variable) +
      ggplot2::coord_flip() +
      r2dii.plot::theme_2dii() +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        legend.position = "none"
      ) +
      ggplot2::facet_wrap(
        ~ forcats::fct_reorder(
          .data[[x_var]],
          .data[[y_var]],
          .fun = max,
          .desc = TRUE
        ),
        scales = "free_y",
        ncol = 1
      ) +
      ggplot2::labs(
        title = paste0(
          y_var,
          " with the highest exposure within ",
          facet_var
        ),
        y = "Exposure"
      )
    p
  }

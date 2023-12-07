#' Title
#'
#' @param data_performers_plot
#' @param x_var
#' @param y_var
#' @param fill_var
#' @param facet_var
#'
#' @return
#' @export
#'
#' @examples
qplot_worst_performers <-
  function(data_performers_plot, x_var, y_var, fill_var, facet_var) {
    fill_colors <- r2dii.colours::palette_1in1000_plot[-c(1:2), ][c(1:length(unique(data_performers_plot[, fill_var]))), ]$hex

    # reorder_within
    p <- ggplot(
      data_performers_plot,
      aes(
        x = !!rlang::sym(x_var),
        y = !!rlang::sym(y_var),
        fill = !!rlang::sym(fill_var)
      )
    ) +
      geom_bar(stat = "identity") +
      geom_text(aes(
        label = scales::unit_format(unit = "M", scale = 1e-6)(.data[[y_var]]),
        hjust = -0.2
      ), stat = "identity") +
      scale_x_reordered() +
      scale_y_continuous(expand = expansion(mult = c(0, .1)), label = scales::unit_format(unit = "M", scale = 1e-6)) +
      ggplot2::scale_color_manual(
        breaks = data_performers_plot[[facet_var]],
        values = fill_colors
      ) +
      # scale_fill_2dii("pacta", colour_groups = data_worst_performers$group_variable) +
      coord_flip() +
      theme_2dii() +
      theme(
        axis.title.y = element_blank(),
        legend.position = "none"
      ) +
      facet_wrap(
        ~ fct_reorder(
          .data[[x_var]],
          .data[[y_var]],
          .fun = max,
          .desc = TRUE
        ),
        scales = "free_y",
        ncol = 1
      ) +
      labs(
        title = paste0(
          y_var,
          " with the highest exposure within ",
          facet_var
        ),
        y = "Exposure"
      )
    p
  }

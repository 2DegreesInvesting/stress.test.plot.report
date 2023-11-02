prepare_for_summary_plot <-
  function(analysis_data,
           group_variable_char,
           value_to_plot_char) {
    data_exposure_plot <-
      prepare_for_exposure_plot(analysis_data, group_variable_char, value_to_plot_char)

    data_summary_plot <- data_exposure_plot |>
      dplyr::group_by(.data$group_variable) |>
      dplyr::summarise(
        group_sum = sum(.data$value_to_plot, na.rm = TRUE),
        group_n = dplyr::n(),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = c("group_sum", "group_n"),
        names_to = "agg_sum_name",
        values_to = "agg_sum_value"
      ) |>
      dplyr::mutate(
        label = case_when(
          .data$agg_sum_name == "group_sum" ~ paste0("Total ", value_to_plot_char, " per ", group_variable_char),
          .data$agg_sum_name == "group_n" ~ "Number of companies per sector"
        )
      ) |>
      dplyr::group_by(agg_sum_name) |>
      dplyr::mutate(perc_of_total = .data$agg_sum_value / sum(.data$agg_sum_value)) |>
      dplyr::ungroup()
    return(data_summary_plot)
  }

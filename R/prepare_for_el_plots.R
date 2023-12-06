prepare_for_el_plots <-
  function(analysis_data_single_run,
           group_variable_char,
           value_to_plot_char) {
    stopifnot(value_to_plot_char == "exposure_at_default")

    data_summary_plot <-
      prepare_for_summary_plot(analysis_data_single_run,
                               group_variable = group_variable_char,
                               value_to_plot = value_to_plot_char)

    data_exp_per_sector <- data_summary_plot |>
      dplyr::select(-c(perc_of_total))|>
      dplyr::filter(agg_sum_name == "group_sum")

    st_expected_loss <- analysis_data_single_run |>
      dplyr::rename(group_variable = !!rlang::sym(group_variable_char)) |>
      dplyr::group_by(group_variable) |>
      dplyr::summarise(
        expected_loss_baseline = sum(expected_loss_baseline, na.rm = TRUE),
        expected_loss_shock = sum(expected_loss_shock, na.rm = TRUE),
        expected_loss_st_diff = expected_loss_baseline - expected_loss_shock
      ) |>
      tidyr::pivot_longer(
        cols = c(
          expected_loss_baseline,
          expected_loss_shock,
          expected_loss_st_diff
        ),
        names_to = "expected_loss_type",
        values_to = "expected_loss_value"
      ) |>
      dplyr::mutate(expected_loss_type = stringr::str_remove(.data$expected_loss_type, "expected_loss_"))

    data_el <-
      inner_join(st_expected_loss, data_exp_per_sector, by = "group_variable") |>
      dplyr::mutate(el_as_perc_exposure = .data$expected_loss_value / .data$agg_sum_value)

    return(data_el)

  }

# DEPRECATED outlier filtering function in use in some ol plots
remove_outliers_legacy <- function(data) {
  out <- data %>%
    dplyr::group_by(.data$group_variable) %>%
    dplyr::mutate(
      is_outlier = (abs(.data$value_to_plot - stats::median(.data$value_to_plot)) > 2 * stats::sd(.data$value_to_plot, na.rm = TRUE))
    )

  out %>%
    dplyr::summarise(is_outlier = sum(is_outlier)) %>%
    glue::glue_data("{is_outlier} outliers are removed from {group_variable} sector data.")

  out <- out %>%
    dplyr::filter(!is_outlier) %>%
    dplyr::select(-is_outlier)
  out
}

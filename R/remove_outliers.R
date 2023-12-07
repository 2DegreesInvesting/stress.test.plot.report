remove_outliers <- function(data) {
  out <- data %>%
    group_by(.data$group_variable) %>%
    mutate(
      is_outlier = (abs(.data$value_to_plot - median(.data$value_to_plot)) > 2 * sd(.data$value_to_plot, na.rm = TRUE))
    )

  out %>%
    summarise(is_outlier = sum(is_outlier)) %>%
    glue::glue_data("{is_outlier} outliers are removed from {group_variable} sector data.")

  out <- out %>%
    filter(!is_outlier) %>%
    select(-is_outlier)
  out
}

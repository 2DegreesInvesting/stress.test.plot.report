



pipeline_trisk_agg_barplots <- function(){

}



prepare_for_trisk_agg_barplots <- function(){

}



draw_trisk_agg_barplots <- function(){
  plot <-
    ggplot(
      plot_data,
      aes(x = ald_business_unit, y = mean, fill = metric)
    ) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.2,
                  position = position_dodge(0.9)
    ) +
    facet_wrap(~metric, scales = "free_y") +
    scale_fill_manual(values = c(
      "Average NPV Rate of change" = "#5D9324",
      "Average PD Difference" = "#BAB6B5"
    )) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    r2dii.plot::theme_2dii() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    labs(y = "Average Value", x = "Business Unit") +
    ggtitle(paste(
      plot_data[1, "shock_scenario"] %>% pull(),
      " - shock year :",
      plot_data[1, "shock_year"] %>% pull()
    ))

  return(plot)
}

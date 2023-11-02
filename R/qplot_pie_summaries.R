qplot_pie_summaries <- function(data_summary_plot, agg_sum_name_chr) {
  data_pie_summary <- data_summary_plot |>
    dplyr::filter(agg_sum_name==agg_sum_name_chr)

  p1 <- ggplot(
    data_pie_summary,
    aes(x = "", y = .data$agg_sum_value, fill = .data$group_variable)
  ) +
    geom_bar(stat = "identity", width = 1) +
    r2dii.colours::scale_fill_2dii("pacta", colour_groups = data_summary_plot$group_variable) +
    coord_polar("y", start = 0) +
    r2dii.plot::theme_2dii() +
    theme(
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      plot.title=element_text(size=8, hjust=0, face="italic", color="black")
    ) +
    # facet_wrap(~label) +
    geom_text(aes(x = 1.6,
                  label=scales::unit_format(unit = "M", scale = 1e-6)(.data$agg_sum_value)))

  p2 <- ggplot(
    data_pie_summary,
    aes(x = "", y = .data$perc_of_total, fill = .data$group_variable)
    ) +
  geom_bar(stat = "identity", width = 1) +
  r2dii.colours::scale_fill_2dii("pacta", colour_groups = data_summary_plot$group_variable) +
  coord_polar("y", start = 0) +
  r2dii.plot::theme_2dii() +
  theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    plot.title=element_text(size=8, hjust=0, face="italic", color="black")
  ) +
  # facet_wrap(~label) +
  geom_text(aes(x = 1.6, label=.data$perc_of_total,
                # ,position = position_stack(vjust = .5)
                ))

  ggpubr::ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

  # (p1+p2) + labs(title=paste("Portfolio composition perc of total, counting type as", agg_sum_name_chr))
}



# install.packages("ggplot2")
# install.packages("ggrepel")
# install.packages("tidyverse")
library(ggplot2)
library(ggrepel)
library(tidyverse)

df <- tibble::tribble(
  ~group_variable, ~agg_sum_name,   ~agg_sum_value,                                     ~label,     ~perc_of_total,
  "Coal",   "group_sum", 1109751326.11743, "Total exposure_at_default per ald_sector", 0.0892833837424385,
  "Oil&Gas",   "group_sum", 1478872690.56386, "Total exposure_at_default per ald_sector",  0.118980491241922,
  "Power",   "group_sum", 9840915272.72111, "Total exposure_at_default per ald_sector",   0.79173612501564
)




# Get the positions
df <- df %>%
  arrange(group_variable) %>%
  mutate(csum = rev(cumsum(rev(perc_of_total))))%>%
  mutate(pos = (perc_of_total)/2 + lead(csum, 1),
         pos = if_else(is.na(pos), (perc_of_total)/2, pos)
  )


ggplot(df, aes(x = "" ,y = .data$perc_of_total, fill = .data$group_variable)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  # r2dii.colours::scale_fill_2dii("pacta", colour_groups = data_summary_plot$group_variable) +
  # coord_polar("y", start = 0) +
  r2dii.plot::theme_2dii() +
  ggrepel::geom_label_repel(
    aes(label=scales::percent_format(accuracy=0.01)(.data$perc_of_total)
        ,y = .data$pos
        #   fill = .data$group_variable,
        #   label = scales::unit_format(unit = "M", scale = 1e-6)(.data$agg_sum_value)
    ),
    size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void()

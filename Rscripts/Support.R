
  #source("Rscripts/FormatData.R")

Supports <- c('Seminars', 'Mentoring', 'Coaching', 'Support Networks', 'Online Resources')
Support_columns <- c(expr(Support_Seminar), expr(Support_Mentoring), expr(Support_Coaching), expr(Support_Network),expr(Support_Resources))
Support_answers <- c("Essential", "Useful", "Not sure", "Not useful")
Support_colors <- c("black", "#666666", "#6BAED6", '#08519C')


# create dataset for plotting per Divisions
pgrdata_Support_for_plotting <- prepare_data_for_plotting(Supports, pgrdata_Support, Support_answers, Support_columns)
staffdata_Support_for_plotting <- prepare_data_for_plotting(Supports, staffdata_Support, Support_answers, Support_columns)

# regroup data split per Division for overall plot
All_pgrdata_Support_for_plotting <- regroup_all_data(pgrdata_Support_for_plotting)
All_staffdata_Support_for_plotting <- regroup_all_data(staffdata_Support_for_plotting)

# circular plot per Division
## pgrdata_Support_plot <- circular_plot_function(pgrdata_Support_for_plotting, Supports, Support_answers, title_plot = 'Support', Support_colors)

# plot regrouped data 
## All_pgrdata_Support_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Support_for_plotting, Supports, Support_answers, Support_colors)

# Horizontal stacked barplot per ORP
pgrdata_Support_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Support_for_plotting, Support_answers, Support_colors, title_legend = NULL, title_plot = "PGR students")
staffdata_Support_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Support_for_plotting, Support_answers, Support_colors, title_legend = NULL, title_plot = "Researchers")

doubleplot_Support <- ggarrange(pgrdata_Support_perORP, 
                                  staffdata_Support_perORP, 
                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Support <- annotate_figure(doubleplot_Support, 
                                        top = text_grob("Support of ORPs", 
                                                        face = "bold", size = 14))
#ggsave("Figures/Support.png", width = 10, height = 9, bg = "white")

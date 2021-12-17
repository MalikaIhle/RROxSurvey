
  #source("Rscripts/FormatData.R")

Supports <- c('Seminars', 'Mentoring', 'Coaching', 'Support Networks', 'Online Resources')
Support_columns <- c(expr(Support_Seminar), expr(Support_Mentoring), expr(Support_Coaching), expr(Support_Network),expr(Support_Resources))
Support_answers <- c("Essential", "Useful", "Not sure", "Not useful")
Support_colors <- c("black", "#666666", "#6BAED6", '#08519C')

# create dataset for plotting per Divisions
pgrdata_Support_for_plotting <- prepare_data_for_plotting(Supports, pgrdata_Support, Support_answers, Support_columns)
allstaffdata_Support_for_plotting <- prepare_data_for_plotting(Supports, allstaffdata_Support, Support_answers, Support_columns)
staffdata_Support_for_plotting <- prepare_data_for_plotting(Supports, staffdata_Support, Support_answers, Support_columns)
supportstaffdata_Support_for_plotting <- prepare_data_for_plotting(Supports, supportstaffdata_Support, Support_answers, Support_columns)
academicdata_Support_for_plotting <- prepare_data_for_plotting(Supports, academicdata_Support, Support_answers, Support_columns)

# regroup data split per Division for overall plot
All_pgrdata_Support_for_plotting <- regroup_all_data(pgrdata_Support_for_plotting)
All_allstaffdata_Support_for_plotting <- regroup_all_data(allstaffdata_Support_for_plotting)
All_staffdata_Support_for_plotting <- regroup_all_data(staffdata_Support_for_plotting)
All_supportstaffdata_Support_for_plotting <- regroup_all_data(supportstaffdata_Support_for_plotting)
All_academicdata_Support_for_plotting <- regroup_all_data(academicdata_Support_for_plotting)

# circular plot per Division
## pgrdata_Support_plot <- circular_plot_function(pgrdata_Support_for_plotting, Supports, Support_answers, title_plot = 'Support', Support_colors)


title_plot_pgr <- paste ("PGR students (N=",sst_pgrdata$Total[sst_pgrdata$Question == "Support"], ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sst_staffdata$Total[sst_staffdata$Question == "Support"], ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Support"], ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sst_academicdata$Total[sst_academicdata$Question == "Support"], ")" , sep="")

# plot regrouped data 
temp <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_pgr, legend_position = "bottom")
shared_legend <- extract_legend(temp)

All_pgrdata_Support_plot <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_pgr, legend_position = "none")
All_staffdata_Support_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_staffdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_staff)
All_supportstaffdata_Support_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_supportstaffdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_supportstaff)
All_academicdata_Support_plot <- horizontal_stacked_barplot_on_regrouped_data_x_right(All_academicdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_academic)

quadrupleplot_All_Support <- egg::ggarrange(All_pgrdata_Support_plot, 
                                            All_staffdata_Support_plot, 
                                            All_supportstaffdata_Support_plot,
                                            All_academicdata_Support_plot,
                                            nrow=1)

quadrupleplot_All_Support_with_legend <- ggpubr::ggarrange(quadrupleplot_All_Support, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/
quadrupleplot_All_Support_with_legend <- annotate_figure(quadrupleplot_All_Support_with_legend, top = text_grob("Support needs", face = "bold", size = 14))
quadrupleplot_All_Support_with_legend
#ggsave("Figures/quadrupleplot_All_Support.png", width = 12, height = 2.5, bg = "white")

# Horizontal stacked barplot per ORP
pgrdata_Support_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_legend = NULL, title_plot = title_plot_pgr)
allstaffdata_Support_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_legend = NULL, title_plot = "Researchers")
staffdata_Support_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_legend = NULL, title_plot = title_plot_staff)
supportstaffdata_Support_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_legend = NULL, title_plot = title_plot_supportstaff)
academicdata_Support_perORP <- horizontal_stack_barplot_per_ORP(academicdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_legend = NULL, title_plot =title_plot_academic)


# doubleplot_Support <- ggpubr::ggarrange(pgrdata_Support_perORP, 
#                                  allstaffdata_Support_perORP, 
#                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
# doubleplot_Support <- annotate_figure(doubleplot_Support, 
#                                        top = text_grob("Support needs", 
#                                                        face = "bold", size = 14))
#ggsave("Figures/Support.png", width = 10, height = 13, bg = "white")



quadrupleplot_Support <- ggpubr::ggarrange(pgrdata_Support_perORP, 
                                            staffdata_Support_perORP, 
                                            supportstaffdata_Support_perORP,
                                            academicdata_Support_perORP,
                                            ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_Support <- annotate_figure(quadrupleplot_Support, 
                                          top = text_grob("Support needs", 
                                                          face = "bold", size = 14))
quadrupleplot_Support
#ggsave("Figures/quadrupleplot_Support.png", width = 10, height = 13, bg = "white")


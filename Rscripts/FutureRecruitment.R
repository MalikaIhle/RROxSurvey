
  #source("Rscripts/FormatData.R")

Criteria
FutureRecruitment_columns <- c(expr(FutureRecruitment_PubNub), expr(FutureRecruitment_PubPrestige), expr(FutureRecruitment_PubQual), expr(FutureRecruitment_Authorship),expr(FutureRecruitment_Citation),
                                expr(FutureRecruitment_Grant),expr(FutureRecruitment_Impact),expr(FutureRecruitment_Teaching),expr(FutureRecruitment_Supervision),expr(FutureRecruitment_Service),
                                expr(FutureRecruitment_Citizenship),expr(FutureRecruitment_Reputation),expr(FutureRecruitment_Collaboration),expr(FutureRecruitment_OpenResearch))
FutureRecruitment_answers <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")
FutureRecruitment_colors <- c("black", "#666666", "#FDE0DD",'#FA9FB5',"#F768A1",'#DD3497')

# create dataset for plotting per Divisions
pgrdata_FutureRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, pgrdata_FutureRecruitment, FutureRecruitment_answers, FutureRecruitment_columns)
allstaffdata_FutureRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, allstaffdata_FutureRecruitment, FutureRecruitment_answers, FutureRecruitment_columns)
staffdata_FutureRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, staffdata_FutureRecruitment, FutureRecruitment_answers, FutureRecruitment_columns)
supportstaffdata_FutureRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, supportstaffdata_FutureRecruitment, FutureRecruitment_answers, FutureRecruitment_columns)
academicdata_FutureRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, academicdata_FutureRecruitment, FutureRecruitment_answers, FutureRecruitment_columns)

# regroup data split per Division for overall plot
All_pgrdata_FutureRecruitment_for_plotting <- regroup_all_data(pgrdata_FutureRecruitment_for_plotting)
All_allstaffdata_FutureRecruitment_for_plotting <- regroup_all_data(allstaffdata_FutureRecruitment_for_plotting)
All_staffdata_FutureRecruitment_for_plotting <- regroup_all_data(staffdata_FutureRecruitment_for_plotting)
All_supportstaffdata_FutureRecruitment_for_plotting <- regroup_all_data(supportstaffdata_FutureRecruitment_for_plotting)
All_academicdata_FutureRecruitment_for_plotting <- regroup_all_data(academicdata_FutureRecruitment_for_plotting)

# Horizontal stacked barplot
title_plot_pgr <- paste ("PGR students (N=",sst_pgrdata$Total[sst_pgrdata$Question == "FutureRecruitment"], ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sst_staffdata$Total[sst_staffdata$Question == "FutureRecruitment"], ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sst_supportstaffdata$Total[sst_supportstaffdata$Question == "FutureRecruitment"], ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sst_academicdata$Total[sst_academicdata$Question == "FutureRecruitment"], ")" , sep="")

temp <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_pgr, legend_position = "bottom")
shared_legend <- extract_legend(temp)
rm(temp)

All_pgrdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_pgr, legend_position = "none")
All_staffdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_staffdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_staff)
All_supportstaffdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_supportstaffdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_supportstaff)
All_academicdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_x_right(All_academicdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_academic)

quadrupleplot_All_FutureRecruitement <- egg::ggarrange(All_pgrdata_FutureRecruitment_plot, 
                                                    All_staffdata_FutureRecruitment_plot, 
                                                    All_supportstaffdata_FutureRecruitment_plot,
                                                    All_academicdata_FutureRecruitment_plot,
                                                    nrow=1)

quadrupleplot_All_FutureRecruitement_with_legend <- ggpubr::ggarrange(quadrupleplot_All_FutureRecruitement, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/

quadrupleplot_All_FutureRecruitement_with_legend <- annotate_figure(quadrupleplot_All_FutureRecruitement_with_legend, top = text_grob("Desired future recruitment criteria", 
                                                                                                                                face = "bold", size = 14))


#ggsave("Figures/quadrupleplot_All_FutureRecruitement.png", width = 15, height = 10, bg = "white")



# circular plot per Division
## pgrdata_FutureRecruitment_plot <- circular_plot_function(pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, title_plot = 'Future Recruitment Criteria', FutureRecruitment_colors)

# plot regrouped data 
## All_pgrdata_FutureRecruitment_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors)

# Horizontal stacked barplot per ORP
pgrdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_pgr)
allstaffdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_FutureRecruitment_for_plotting,Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = "Researchers")
staffdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(staffdata_FutureRecruitment_for_plotting, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_staff)
supportstaffdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_FutureRecruitment_for_plotting, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_supportstaff)
academicdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(academicdata_FutureRecruitment_for_plotting, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_academic)

quadrupleplot_FutureRecruitment <- ggpubr::ggarrange(pgrdata_FutureRecruitment_perORP,
                                                      staffdata_FutureRecruitment_perORP,
                                                      supportstaffdata_FutureRecruitment_perORP,
                                                      academicdata_FutureRecruitment_perORP,
                                                      ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_FutureRecruitment <- annotate_figure(quadrupleplot_FutureRecruitment,
                                                    top = text_grob("Desired future recruitment criteria",
                                                                    face = "bold", size = 14))
quadrupleplot_FutureRecruitment
#ggsave("Figures/quadruple_FutureRecruitment.png", width = 12, height = 20, bg = "white")


  #source("Rscripts/FormatData.R")

Criteria
CurrentRecruitment_columns <- c(expr(CurrentRecruitment_PubNub), expr(CurrentRecruitment_PubPrestige), expr(CurrentRecruitment_PubQual), expr(CurrentRecruitment_Authorship),expr(CurrentRecruitment_Citation),
                      expr(CurrentRecruitment_Grant),expr(CurrentRecruitment_Impact),expr(CurrentRecruitment_Teaching),expr(CurrentRecruitment_Supervision),expr(CurrentRecruitment_Service),
                      expr(CurrentRecruitment_Citizenship),expr(CurrentRecruitment_Reputation),expr(CurrentRecruitment_Collaboration),expr(CurrentRecruitment_OpenResearch))
CurrentRecruitment_answers <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")
CurrentRecruitment_colors <- c("black", "#666666", "#FDE0DD",'#FA9FB5',"#F768A1",'#DD3497')

# create dataset for plotting per Divisions
pgrdata_CurrentRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, pgrdata_CurrentRecruitment, CurrentRecruitment_answers, CurrentRecruitment_columns)
allstaffdata_CurrentRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, allstaffdata_CurrentRecruitment, CurrentRecruitment_answers, CurrentRecruitment_columns)
staffdata_CurrentRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, staffdata_CurrentRecruitment, CurrentRecruitment_answers, CurrentRecruitment_columns)
supportstaffdata_CurrentRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, supportstaffdata_CurrentRecruitment, CurrentRecruitment_answers, CurrentRecruitment_columns)
academicdata_CurrentRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, academicdata_CurrentRecruitment, CurrentRecruitment_answers, CurrentRecruitment_columns)

# regroup data split per Division for overall plot
All_pgrdata_CurrentRecruitment_for_plotting <- regroup_all_data(pgrdata_CurrentRecruitment_for_plotting)
All_allstaffdata_CurrentRecruitment_for_plotting <- regroup_all_data(allstaffdata_CurrentRecruitment_for_plotting)
All_staffdata_CurrentRecruitment_for_plotting <- regroup_all_data(staffdata_CurrentRecruitment_for_plotting)
All_supportstaffdata_CurrentRecruitment_for_plotting <- regroup_all_data(supportstaffdata_CurrentRecruitment_for_plotting)
All_academicdata_CurrentRecruitment_for_plotting <- regroup_all_data(academicdata_CurrentRecruitment_for_plotting)

# Horizontal stacked barplot
title_plot_pgr <- paste ("PGR students (N=",sst_pgrdata$Total[sst_pgrdata$Question == "CurrentRecruitment"], ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sst_staffdata$Total[sst_staffdata$Question == "CurrentRecruitment"], ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sst_supportstaffdata$Total[sst_supportstaffdata$Question == "CurrentRecruitment"], ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sst_academicdata$Total[sst_academicdata$Question == "CurrentRecruitment"], ")" , sep="")

temp <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors, title_plot = title_plot_pgr, legend_position = "bottom")
shared_legend <- extract_legend(temp)
rm(temp)

All_pgrdata_CurrentRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors, title_plot = title_plot_pgr, legend_position = "none")
All_staffdata_CurrentRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_staffdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors, title_plot = title_plot_staff)
All_supportstaffdata_CurrentRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_supportstaffdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors, title_plot = title_plot_supportstaff)
All_academicdata_CurrentRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_x_right(All_academicdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors, title_plot = title_plot_academic)

quadrupleplot_All_CurrentRecruitement <- egg::ggarrange(All_pgrdata_CurrentRecruitment_plot, 
                                               All_staffdata_CurrentRecruitment_plot, 
                                               All_supportstaffdata_CurrentRecruitment_plot,
                                               All_academicdata_CurrentRecruitment_plot,
                                               nrow=1)

quadrupleplot_All_CurrentRecruitement_with_legend <- ggpubr::ggarrange(quadrupleplot_All_CurrentRecruitement, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/

quadrupleplot_All_CurrentRecruitement_with_legend <- annotate_figure(quadrupleplot_All_CurrentRecruitement_with_legend, top = text_grob("Perceived current recruitment criteria", 
                                                                     face = "bold", size = 14))


#ggsave("Figures/quadrupleplot_All_CurrentRecruitement.png", width = 15, height = 10, bg = "white")



# circular plot per Division
## pgrdata_CurrentRecruitment_plot <- circular_plot_function(pgrdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, title_plot = 'Current Recruitment Criteria', CurrentRecruitment_colors)

# plot regrouped data 
## All_pgrdata_CurrentRecruitment_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors)

# Horizontal stacked barplot per ORP
pgrdata_CurrentRecruitment_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors, title_legend = NULL, title_plot = title_plot_pgr)
allstaffdata_CurrentRecruitment_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_CurrentRecruitment_for_plotting,Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors, title_legend = NULL, title_plot = "Researchers")
staffdata_CurrentRecruitment_perORP <- horizontal_stack_barplot_per_ORP(staffdata_CurrentRecruitment_for_plotting, Criteria,CurrentRecruitment_answers, CurrentRecruitment_colors, title_legend = NULL, title_plot = title_plot_staff)
supportstaffdata_CurrentRecruitment_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_CurrentRecruitment_for_plotting, Criteria,CurrentRecruitment_answers, CurrentRecruitment_colors, title_legend = NULL, title_plot = title_plot_supportstaff)
academicdata_CurrentRecruitment_perORP <- horizontal_stack_barplot_per_ORP(academicdata_CurrentRecruitment_for_plotting, Criteria,CurrentRecruitment_answers, CurrentRecruitment_colors, title_legend = NULL, title_plot = title_plot_academic)

quadrupleplot_CurrentRecruitment <- ggpubr::ggarrange(pgrdata_CurrentRecruitment_perORP,
                                  staffdata_CurrentRecruitment_perORP,
                                  supportstaffdata_CurrentRecruitment_perORP,
                                  academicdata_CurrentRecruitment_perORP,
                                  ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_CurrentRecruitment <- annotate_figure(quadrupleplot_CurrentRecruitment,
                                        top = text_grob("Perceived current recruitment criteria",
                                                        face = "bold", size = 14))
quadrupleplot_CurrentRecruitment
#ggsave("Figures/quadruple_CurrentRecruitment.png", width = 12, height = 20, bg = "white")

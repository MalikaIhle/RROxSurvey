
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
alldata_Support_for_plotting <- prepare_data_for_plotting(Supports, data_Support, Support_answers, Support_columns)
alldata_but_academic_Support_for_plotting <- rbind(pgrdata_Support_for_plotting,staffdata_Support_for_plotting,supportstaffdata_Support_for_plotting)

# regroup data split per Division for overall plot
All_pgrdata_Support_for_plotting <- regroup_all_data(pgrdata_Support_for_plotting)
All_allstaffdata_Support_for_plotting <- regroup_all_data(allstaffdata_Support_for_plotting)
All_staffdata_Support_for_plotting <- regroup_all_data(staffdata_Support_for_plotting)
All_supportstaffdata_Support_for_plotting <- regroup_all_data(supportstaffdata_Support_for_plotting)
All_academicdata_Support_for_plotting <- regroup_all_data(academicdata_Support_for_plotting)
All_alldata_but_academic_Support_for_plotting <- regroup_all_data(alldata_but_academic_Support_for_plotting)

# circular plot per Division
## pgrdata_Support_plot <- circular_plot_function(pgrdata_Support_for_plotting, Supports, Support_answers, title_plot = 'Support', Support_colors)

# Horizontal stack bar plot on regrouped data (all div)

title_plot_pgr_regrouped <- paste ("PGR students (N=",sst_pgrdata$Total[sst_pgrdata$Question == "Support"], ")" , sep="")
title_plot_allstaff_regrouped <- paste ("Researchers (N=",sum(as.numeric(sst_allstaffdata$Total[sst_allstaffdata$Question == "Support"])), ")" , sep="")
title_plot_staff_regrouped <- paste ("Research staff (N=",sst_staffdata$Total[sst_staffdata$Question == "Support"], ")" , sep="")
title_plot_supportstaff_regrouped <- paste ("Research support staff (N=",sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Support"], ")" , sep="")
title_plot_academic_regrouped <- paste ("Academics (N=",sst_academicdata$Total[sst_academicdata$Question == "Support"], ")" , sep="")
title_plot_nonacademic_regrouped <- paste ("Non-Academics (N=",sum(as.numeric(sst_staffdata$Total[sst_staffdata$Question == "Support"]),
                                                                   as.numeric(sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Support"]),
                                                                   as.numeric(sst_pgrdata$Total[sst_pgrdata$Question == "Support"])), ")" , sep="")

# plot regrouped data 
temp <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_pgr_regrouped, legend_position = "bottom")
shared_legend <- extract_legend(temp)

All_pgrdata_Support_plot <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_pgr_regrouped, legend_position = "none")
All_allstaffdata_Support_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_allstaffdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_allstaff_regrouped)
All_staffdata_Support_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_staffdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_staff_regrouped)
All_supportstaffdata_Support_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_supportstaffdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_supportstaff_regrouped)
All_academicdata_Support_plot <- horizontal_stacked_barplot_on_regrouped_data_x_right(All_academicdata_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_academic_regrouped)
All_nonacademicdata_Support_plot <- horizontal_stacked_barplot_on_regrouped_data(All_alldata_but_academic_Support_for_plotting, Supports, Support_answers, Support_colors, title_plot = title_plot_nonacademic_regrouped, legend_position = "none")



doubleplot_All_Support <- egg::ggarrange(All_pgrdata_Support_plot, 
                                          All_allstaffdata_Support_plot, 
                                          nrow=1)

doubleplot_All_Support_with_legend <- ggpubr::ggarrange(doubleplot_All_Support, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/

doubleplot_All_Support_with_legend <- annotate_figure(doubleplot_All_Support_with_legend, top = text_grob("Support needs", face = "bold", size = 14))

doubleplot_All_Support_with_legend
#ggsave(here::here("Figures", "Round12_Double_AllDiv_Support.png"), width = 15, height = 10, bg = "white")



doubleplot_All_AcademicvsNonAcademic_Support <- egg::ggarrange(All_nonacademicdata_Support_plot, 
                                                               All_academicdata_Support_plot, nrow=1)

doubleplot_All_AcademicvsNonAcademic_Support_with_legend <- ggpubr::ggarrange(doubleplot_All_AcademicvsNonAcademic_Support, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/

doubleplot_All_AcademicvsNonAcademic_Support_with_legend <- annotate_figure(doubleplot_All_AcademicvsNonAcademic_Support_with_legend, top = text_grob("Support needs", face = "bold", size = 14))

doubleplot_All_AcademicvsNonAcademic_Support_with_legend
#ggsave(here::here("Figures", "Round12_Double_AllDiv_AcademicvsNonAcademic_Support.png"), width = 10, height = 3, bg = "white")





quadrupleplot_All_Support <- egg::ggarrange(All_pgrdata_Support_plot, 
                                             All_staffdata_Support_plot, 
                                             All_supportstaffdata_Support_plot,
                                             All_academicdata_Support_plot,
                                             nrow=1)

quadrupleplot_All_Support_with_legend <- ggpubr::ggarrange(quadrupleplot_All_Support, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/
quadrupleplot_All_Support_with_legend <- annotate_figure(quadrupleplot_All_Support_with_legend, top = text_grob("Support needs", face = "bold", size = 14))
quadrupleplot_All_Support_with_legend
#ggsave(here::here("Figures", "Round12_Quadruple_AllDiv_Support.png"), width = 12, height = 4, bg = "white")


## all data pooled across Div and target pop
All_Data_Support <- rbind(
  pgrdata_Support,
  staffdata_Support,
  supportstaffdata_Support,
  academicdata_Support)

All_Split_Support_for_plotting <- prepare_data_for_plotting(Supports, All_Data_Support, Support_answers, Support_columns)
All_Grouped_Support_for_plotting <- regroup_all_data(All_Split_Support_for_plotting)

title_plot_All_Support <- paste ("Support needed for adoption of ORPs
(all researchers, N=",(as.numeric(sst_data$Total[sst_data$Question == "Support"])), ")" , sep="")


All_Grouped_Support_plot <- horizontal_stacked_barplot_on_regrouped_data(All_Grouped_Support_for_plotting, 
                                                                          Supports, 
                                                                          Support_answers, 
                                                                          Support_colors, 
                                                                          title_plot = title_plot_All_Support, 
                                                                          legend_position = "bottom")
All_Grouped_Support_plot
#ggsave(here::here("Figures", "Round12_Single_Support.png"), width = 10, height = 4, bg = "white")



# Horizontal stacked barplot per ORP, Div split
Plotted_Div <- c("MSD", "MPLS","SSD", "Hum")
title_plot_pgr <- paste ("PGR students (N=",sum(as.numeric(sst_pgrdata[sst_pgrdata$Question == "Support", Plotted_Div ])), ")" , sep="")
title_plot_allstaff <- paste ("Researchers (N=",sum(as.numeric(sst_allstaffdata[sst_allstaffdata$Question == "Support", Plotted_Div ])), ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "Support", Plotted_Div ])), ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sum(as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "Support", Plotted_Div ])), ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sum(as.numeric(sst_academicdata[sst_academicdata$Question == "Support", Plotted_Div ])), ")" , sep="")
title_plot_alldata <- paste ("PGR students and all researchers combined (N=",sum(as.numeric(sst_data[sst_data$Question == "Support", Plotted_Div ])),")" , sep="")
title_plot_allbutacademic <- paste ("Non-Academics (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "Support", Plotted_Div ]),
                                                                    as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "Support", Plotted_Div ]),
                                                                    as.numeric(sst_pgrdata[sst_pgrdata$Question == "Support", Plotted_Div ])), ")" , sep="")


pgrdata_Support_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Support_for_plotting, Plotted_Div, Supports, Support_answers, Support_colors, title_legend = NULL, title_plot = title_plot_pgr)
allstaffdata_Support_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_Support_for_plotting, Plotted_Div, Supports,Support_answers, Support_colors, title_legend = NULL, title_plot = title_plot_allstaff)
staffdata_Support_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Support_for_plotting, Plotted_Div, Supports,Support_answers, Support_colors, title_legend = NULL, title_plot = title_plot_staff)
supportstaffdata_Support_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_Support_for_plotting, Plotted_Div, Supports,Support_answers, Support_colors, title_legend = NULL, title_plot = title_plot_supportstaff)
academicdata_Support_perORP <- horizontal_stack_barplot_per_ORP(academicdata_Support_for_plotting, Plotted_Div, Supports,Support_answers, Support_colors, title_legend = NULL, title_plot =title_plot_academic)
nonacademicdata_Support_perORP <- horizontal_stack_barplot_per_ORP(alldata_but_academic_Support_for_plotting,  Plotted_Div, Supports,Support_answers, Support_colors, title_legend = NULL, title_plot = title_plot_allbutacademic)

alldata_Support_perORP <- horizontal_stack_barplot_per_ORP(alldata_Support_for_plotting,  Plotted_Div, Supports,Support_answers, Support_colors, title_legend = NULL, title_plot = title_plot_alldata)
alldata_Support_perORP
#ggsave(here::here("Figures", "Round12_Single_splitDiv_Support.png"), width = 6, height = 10, bg = "white")


doubleplot_Support <- ggpubr::ggarrange(pgrdata_Support_perORP,
                                         allstaffdata_Support_perORP,
                                         ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Support <- annotate_figure(doubleplot_Support,
                                       top = text_grob("Support needs",
                                                       face = "bold", size = 14))
doubleplot_Support
#ggsave(here::here("Figures", "Round12_Double_SplitDiv_Support.png"), width = 10, height = 10, bg = "white")



doubleplot_Support_ac_vs_non_ac <- ggpubr::ggarrange(nonacademicdata_Support_perORP,
                                                     academicdata_Support_perORP,
                                        ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Support_ac_vs_non_ac <- annotate_figure(doubleplot_Support_ac_vs_non_ac,
                                      top = text_grob("Support needs",
                                                      face = "bold", size = 14))
doubleplot_Support_ac_vs_non_ac
#ggsave(here::here("Figures", "Round12_Double_SplitDiv_Academic_vs_NonAcademic_Support.png"), width = 10, height = 10, bg = "white")





quadrupleplot_Support <- ggpubr::ggarrange(pgrdata_Support_perORP, 
                                            staffdata_Support_perORP, 
                                            supportstaffdata_Support_perORP,
                                            academicdata_Support_perORP,
                                            ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_Support <- annotate_figure(quadrupleplot_Support, 
                                          top = text_grob("Support needs", 
                                                          face = "bold", size = 14))
quadrupleplot_Support
#ggsave(here::here("Figures", "Round12_Quadruple_SplitDiv_Support.png"), width = 10, height = 13, bg = "white")


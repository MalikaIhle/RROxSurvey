
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
alldata_FutureRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, data_FutureRecruitment, FutureRecruitment_answers, FutureRecruitment_columns)

x <- rbind(pgrdata_FutureRecruitment,staffdata_FutureRecruitment,supportstaffdata_FutureRecruitment,academicdata_FutureRecruitment)
All_Data_But_Academic_FutureRecruitment_for_plotting <-  prepare_data_for_plotting(Criteria, x, FutureRecruitment_answers, FutureRecruitment_columns)


# regroup data split per Division for overall plot
All_pgrdata_FutureRecruitment_for_plotting <- regroup_all_data(pgrdata_FutureRecruitment_for_plotting)
All_allstaffdata_FutureRecruitment_for_plotting <- regroup_all_data(allstaffdata_FutureRecruitment_for_plotting)
All_staffdata_FutureRecruitment_for_plotting <- regroup_all_data(staffdata_FutureRecruitment_for_plotting)
All_supportstaffdata_FutureRecruitment_for_plotting <- regroup_all_data(supportstaffdata_FutureRecruitment_for_plotting)
All_academicdata_FutureRecruitment_for_plotting <- regroup_all_data(academicdata_FutureRecruitment_for_plotting)

# circular plot per Division
## pgrdata_FutureRecruitment_plot <- circular_plot_function(pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, title_plot = 'Current Recruitment Criteria', FutureRecruitment_colors)

# plot regrouped data 
## All_pgrdata_FutureRecruitment_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors)

# Horizontal stacked barplot on regrouped data (All Div)
title_plot_Current_pgr_regrouped <- paste ("PGR students (N=",sst_pgrdata$Total[sst_pgrdata$Question == "FutureRecruitment"], ")" , sep="")
title_plot_Current_allstaff_regrouped <- paste ("Researchers (N=",sum(as.numeric(sst_allstaffdata$Total[sst_allstaffdata$Question == "FutureRecruitment"])), ")" , sep="")
title_plot_Current_staff_regrouped <- paste ("Research staff (N=",sst_staffdata$Total[sst_staffdata$Question == "FutureRecruitment"], ")" , sep="")
title_plot_Current_supportstaff_regrouped <- paste ("Research support staff (N=",sst_supportstaffdata$Total[sst_supportstaffdata$Question == "FutureRecruitment"], ")" , sep="")
title_plot_Current_academic_regrouped <- paste ("Academics (N=",sst_academicdata$Total[sst_academicdata$Question == "FutureRecruitment"], ")" , sep="")



temp <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Current_pgr_regrouped, legend_position = "bottom")
shared_legend <- extract_legend(temp)
rm(temp)

All_pgrdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Current_pgr_regrouped, legend_position = "none")
All_allstaffdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_allstaffdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Current_allstaff_regrouped)
All_staffdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_staffdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Current_staff_regrouped)
All_supportstaffdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_supportstaffdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Current_supportstaff_regrouped)
All_academicdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_x_right(All_academicdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Current_academic_regrouped)

doubleplot_All_FutureRecruitment <- egg::ggarrange(All_pgrdata_FutureRecruitment_plot, 
                                                    All_allstaffdata_FutureRecruitment_plot, 
                                                    nrow=1)

doubleplot_All_FutureRecruitment_with_legend <- ggpubr::ggarrange(doubleplot_All_FutureRecruitment, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/

doubleplot_All_FutureRecruitment_with_legend <- annotate_figure(doubleplot_All_FutureRecruitment_with_legend, top = text_grob("Desired future recruitment criteria", 
                                                                                                                                face = "bold", size = 14))

doubleplot_All_FutureRecruitment_with_legend
#ggsave(here::here("Figures", "Round12_Double_AllDiv_FutureRecruitment.png"), width = 15, height = 10, bg = "white")


quadrupleplot_All_FutureRecruitment <- egg::ggarrange(All_pgrdata_FutureRecruitment_plot, 
                                                       All_staffdata_FutureRecruitment_plot, 
                                                       All_supportstaffdata_FutureRecruitment_plot,
                                                       All_academicdata_FutureRecruitment_plot,
                                                       nrow=1)

quadrupleplot_All_FutureRecruitment_with_legend <- ggpubr::ggarrange(quadrupleplot_All_FutureRecruitment, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/

quadrupleplot_All_FutureRecruitment_with_legend <- annotate_figure(quadrupleplot_All_FutureRecruitment_with_legend, top = text_grob("Desired future recruitment criteria", 
                                                                                                                                      face = "bold", size = 14))

quadrupleplot_All_FutureRecruitment_with_legend
#ggsave(here::here("Figures", "Round12_Quadruple_AllDiv_FutureRecruitment.png"), width = 15, height = 10, bg = "white")




# Horizontal stacked barplot - split Div

Plotted_Div <- c("MSD", "MPLS","SSD", "Hum")
title_plot_Current_pgr <- paste ("PGR students (N=",sum(as.numeric(sst_pgrdata[sst_pgrdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")
title_plot_Current_allstaff <- paste ("Researchers (N=",sum(as.numeric(sst_allstaffdata[sst_allstaffdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")
title_plot_Current_staff <- paste ("Research staff (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")
title_plot_Current_supportstaff <- paste ("Research support staff (N=",sum(as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")
title_plot_Current_academic <- paste ("Academics (N=",sum(as.numeric(sst_academicdata[sst_academicdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")
title_plot_Current_alldata <- paste ("PGR students and all researchers combined (N=",sum(as.numeric(sst_data[sst_data$Question == "FutureRecruitment", Plotted_Div ])),")" , sep="")
title_plot_Current_allbutacademic <- paste ("Non-Academics (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "FutureRecruitment", Plotted_Div ]),
                                                                    as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "FutureRecruitment", Plotted_Div ]),
                                                                    as.numeric(sst_pgrdata[sst_pgrdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")


pgrdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_FutureRecruitment_for_plotting, Plotted_Div, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Current_pgr)
allstaffdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_FutureRecruitment_for_plotting, Plotted_Div, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Current_allstaff)
staffdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(staffdata_FutureRecruitment_for_plotting,  Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Current_staff)
supportstaffdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_FutureRecruitment_for_plotting,  Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Current_supportstaff)
academicdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(academicdata_FutureRecruitment_for_plotting,  Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Current_academic)
All_Data_But_Academic_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(All_Data_But_Academic_FutureRecruitment_for_plotting,  Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Current_allbutacademic)

alldata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(alldata_FutureRecruitment_for_plotting,  Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Current_alldata)
alldata_FutureRecruitment_perORP
#ggsave(here::here("Figures", "Round12_Single_splitDiv_FutureRecruitment.png"), width = 6, height = 20, bg = "white")

doubleplot_FutureRecruitment <- ggpubr::ggarrange(pgrdata_FutureRecruitment_perORP,
                                                   allstaffdata_FutureRecruitment_perORP,
                                                   ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_FutureRecruitment <- annotate_figure(doubleplot_FutureRecruitment,
                                                 top = text_grob("Desired future recruitment criteria",
                                                                 face = "bold", size = 14))
doubleplot_FutureRecruitment
#ggsave(here::here("Figures", "Round12_Double_splitDiv_FutureRecruitment.png"), width = 12, height = 20, bg = "white")


doubleplotAcademicvsNonAc_FutureRecruitment <- ggpubr::ggarrange(All_Data_But_Academic_FutureRecruitment_perORP,
                                                                  academicdata_FutureRecruitment_perORP,
                                                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplotAcademicvsNonAc_FutureRecruitment <- annotate_figure(doubleplotAcademicvsNonAc_FutureRecruitment,
                                                                top = text_grob("Desired future recruitment criteria",
                                                                                face = "bold", size = 14))
doubleplotAcademicvsNonAc_FutureRecruitment
#ggsave(here::here("Figures", "Round12_DoubleAcademicvsNonAcademic_splitDiv_FutureRecruitment.png"), width = 12, height = 20, bg = "white")



quadrupleplot_FutureRecruitment <- ggpubr::ggarrange(pgrdata_FutureRecruitment_perORP,
                                                      staffdata_FutureRecruitment_perORP,
                                                      supportstaffdata_FutureRecruitment_perORP,
                                                      academicdata_FutureRecruitment_perORP,
                                                      ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_FutureRecruitment <- annotate_figure(quadrupleplot_FutureRecruitment,
                                                    top = text_grob("Desired future recruitment criteria",
                                                                    face = "bold", size = 14))
quadrupleplot_FutureRecruitment
#ggsave(here::here("Figures", "Round12_Quadruple_splitDiv_FutureRecruitment.png"), width = 12, height = 20, bg = "white")

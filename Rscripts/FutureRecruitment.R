
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

All_Data_But_Academic_FutureRecruitment_for_plotting <- rbind(pgrdata_FutureRecruitment_for_plotting,allstaffdata_FutureRecruitment_for_plotting)

# regroup data split per Division for overall plot
All_pgrdata_FutureRecruitment_for_plotting <- regroup_all_data(pgrdata_FutureRecruitment_for_plotting)
All_allstaffdata_FutureRecruitment_for_plotting <- regroup_all_data(allstaffdata_FutureRecruitment_for_plotting)
All_staffdata_FutureRecruitment_for_plotting <- regroup_all_data(staffdata_FutureRecruitment_for_plotting)
All_supportstaffdata_FutureRecruitment_for_plotting <- regroup_all_data(supportstaffdata_FutureRecruitment_for_plotting)
All_academicdata_FutureRecruitment_for_plotting <- regroup_all_data(academicdata_FutureRecruitment_for_plotting)

# Horizontal stacked barplot

Plotted_Div <- c("MSD", "MPLS","SSD", "Hum")
title_plot_Future_pgr <- paste ("PGR students (N=",sum(as.numeric(sst_pgrdata[sst_pgrdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")
title_plot_Future_allstaff <- paste ("Researchers (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "FutureRecruitment", Plotted_Div ]),
                                                    as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "FutureRecruitment", Plotted_Div ]),
                                                    as.numeric(sst_academicdata[sst_academicdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")
title_plot_Future_staff <- paste ("Research staff (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")
title_plot_Future_supportstaff <- paste ("Research support staff (N=",sum(as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")
title_plot_Future_academic <- paste ("Academics (N=",sum(as.numeric(sst_academicdata[sst_academicdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")
title_plot_Future_alldata <- paste ("PGR students and all researchers combined (N=",sum(as.numeric(sst_pgrdata[sst_pgrdata$Question == "FutureRecruitment", Plotted_Div ]),
                                                                                 as.numeric(sst_staffdata[sst_staffdata$Question == "FutureRecruitment", Plotted_Div ]),
                                                                                 as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "FutureRecruitment", Plotted_Div ]),
                                                                                 as.numeric(sst_academicdata[sst_academicdata$Question == "FutureRecruitment", Plotted_Div ])),")" , sep="")
title_plot_Future_allbutacademic <- paste ("Non-Academics (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "FutureRecruitment", Plotted_Div ]),
                                                            as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "FutureRecruitment", Plotted_Div ]),
                                                            as.numeric(sst_pgrdata[sst_pgrdata$Question == "FutureRecruitment", Plotted_Div ])), ")" , sep="")

temp <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Future_pgr, legend_position = "bottom")
shared_legend <- extract_legend(temp)
rm(temp)

All_pgrdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Future_pgr, legend_position = "none")
All_allstaffdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_allstaffdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Future_allstaff)
All_staffdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_staffdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Future_staff)
All_supportstaffdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_supportstaffdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Future_supportstaff)
All_academicdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_x_right(All_academicdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = title_plot_Future_academic)

doubleplot_All_FutureRecruitment <- egg::ggarrange(All_pgrdata_FutureRecruitment_plot, 
                                                     All_allstaffdata_FutureRecruitment_plot, 
                                                     nrow=1)

doubleplot_All_FutureRecruitment_with_legend <- ggpubr::ggarrange(doubleplot_All_FutureRecruitment, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/

doubleplot_All_FutureRecruitment_with_legend <- annotate_figure(doubleplot_All_FutureRecruitment_with_legend, top = text_grob("Desired future recruitment criteria", 
                                                                                                                                  face = "bold", size = 14))

doubleplot_All_FutureRecruitment_with_legend
#ggsave("Figures/Round12_Double_AllDiv_FutureRecruitment.png", width = 15, height = 10, bg = "white")

quadrupleplot_All_FutureRecruitment <- egg::ggarrange(All_pgrdata_FutureRecruitment_plot, 
                                                    All_staffdata_FutureRecruitment_plot, 
                                                    All_supportstaffdata_FutureRecruitment_plot,
                                                    All_academicdata_FutureRecruitment_plot,
                                                    nrow=1)

quadrupleplot_All_FutureRecruitment_with_legend <- ggpubr::ggarrange(quadrupleplot_All_FutureRecruitment, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/

quadrupleplot_All_FutureRecruitment_with_legend <- annotate_figure(quadrupleplot_All_FutureRecruitment_with_legend, top = text_grob("Desired future recruitment criteria", 
                                                                                                                                face = "bold", size = 14))

quadrupleplot_All_FutureRecruitment_with_legend
# ggsave("Figures/Round12_Quadruple_AllDiv_FutureRecruitment.png", width = 15, height = 10, bg = "white")



# circular plot per Division
## pgrdata_FutureRecruitment_plot <- circular_plot_function(pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, title_plot = 'Future Recruitment Criteria', FutureRecruitment_colors)

# plot regrouped data 
## All_pgrdata_FutureRecruitment_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors)

# Horizontal stacked barplot per ORP
pgrdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_FutureRecruitment_for_plotting, Plotted_Div, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_pgr)
allstaffdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_FutureRecruitment_for_plotting,Plotted_Div, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_allstaff)
staffdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(staffdata_FutureRecruitment_for_plotting, Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_staff)
supportstaffdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_FutureRecruitment_for_plotting, Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_supportstaff)
academicdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(academicdata_FutureRecruitment_for_plotting, Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_academic)
All_Data_But_Academic_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(All_Data_But_Academic_FutureRecruitment_for_plotting,  Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_allbutacademic)

alldata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(alldata_FutureRecruitment_for_plotting,  Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_alldata)
# ggsave("Figures/Round12_Single_splitDiv_FutureRecruitment.png", width = 6, height = 20, bg = "white")

doubleplot_FutureRecruitment <- ggpubr::ggarrange(pgrdata_FutureRecruitment_perORP,
                                                   allstaffdata_FutureRecruitment_perORP,
                                                   ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_FutureRecruitment <- annotate_figure(doubleplot_FutureRecruitment,
                                                 top = text_grob("Desired future recruitment criteria",
                                                                 face = "bold", size = 14))
doubleplot_FutureRecruitment
# ggsave("Figures/Round12_Double_splitDiv_FutureRecruitment.png", width = 12, height = 20, bg = "white")


doubleplotAcademicvsNonAc_FutureRecruitment <- ggpubr::ggarrange(All_Data_But_Academic_FutureRecruitment_perORP,
                                                                  academicdata_FutureRecruitment_perORP,
                                                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplotAcademicvsNonAc_FutureRecruitment <- annotate_figure(doubleplotAcademicvsNonAc_FutureRecruitment,
                                                                top = text_grob("Desired future recruitment criteria",
                                                                                face = "bold", size = 14))
doubleplotAcademicvsNonAc_FutureRecruitment
# ggsave("Figures/Round12_DoubleAcademicvsNonAcademic_splitDiv_FutureRecruitment.png", width = 12, height = 20, bg = "white")


quadrupleplot_FutureRecruitment <- ggpubr::ggarrange(pgrdata_FutureRecruitment_perORP,
                                                      staffdata_FutureRecruitment_perORP,
                                                      supportstaffdata_FutureRecruitment_perORP,
                                                      academicdata_FutureRecruitment_perORP,
                                                      ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_FutureRecruitment <- annotate_figure(quadrupleplot_FutureRecruitment,
                                                    top = text_grob("Desired future recruitment criteria",
                                                                    face = "bold", size = 14))
quadrupleplot_FutureRecruitment
# ggsave("Figures/Round12_Quadruple_splitDiv_FutureRecruitment.png", width = 12, height = 20, bg = "white")



# Horizontal stacked barplot for ORP only
All_Data_But_Academic_FutureRecruitment_forORP <- horizontal_stack_barplot_per_ORP(All_Data_But_Academic_FutureRecruitment_for_plotting[All_Data_But_Academic_FutureRecruitment_for_plotting$LabelIndiv == 'Open research practices',]
                                                                                   ,  Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_allbutacademic)
academicdata_FutureRecruitment_forORP <- horizontal_stack_barplot_per_ORP(academicdata_FutureRecruitment_for_plotting[academicdata_FutureRecruitment_for_plotting$LabelIndiv == 'Open research practices',]
                                                                          , Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_academic)



doubleplotAcademicvsNonAc_FutureRecruitment_forORP <- ggpubr::ggarrange(All_Data_But_Academic_FutureRecruitment_forORP,
                                                                 academicdata_FutureRecruitment_forORP,
                                                                 ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplotAcademicvsNonAc_FutureRecruitment_forORP <- annotate_figure(doubleplotAcademicvsNonAc_FutureRecruitment_forORP,
                                                               top = text_grob("Desired future recruitment criteria",
                                                                               face = "bold", size = 14))
doubleplotAcademicvsNonAc_FutureRecruitment_forORP
# ggsave("Figures/Round12_DoubleAcademicvsNonAcademic_splitDiv_FutureRecruitment_forORP.png", width = 10, height = 4, bg = "white")




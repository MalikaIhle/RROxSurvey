  source("Rscripts/FormatData.R")
  source("Rscripts/CurrentRecruitment.R")
  source("Rscripts/FutureRecruitment.R")


# Current Recruitment
All_Data_CurrentRecruitment <- rbind(
pgrdata_CurrentRecruitment,
staffdata_CurrentRecruitment,
supportstaffdata_CurrentRecruitment,
academicdata_CurrentRecruitment)

All_Split_CurrentRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, All_Data_CurrentRecruitment, CurrentRecruitment_answers, CurrentRecruitment_columns)
All_Grouped_CurrentRecruitment_for_plotting <- regroup_all_data(All_Split_CurrentRecruitment_for_plotting)


title_plot_All_CurrentRecruitment <- paste ("Perceived current recruitment criteria
(all researchers, N=",(as.numeric(sst_pgrdata$Total[sst_pgrdata$Question == "CurrentRecruitment"])+
                                                                     as.numeric(sst_staffdata$Total[sst_staffdata$Question == "CurrentRecruitment"])+
                                                                     as.numeric(sst_supportstaffdata$Total[sst_supportstaffdata$Question == "CurrentRecruitment"])+
                                                                     as.numeric(sst_academicdata$Total[sst_academicdata$Question == "CurrentRecruitment"])),
                                              ")" , sep="")


All_Grouped_CurrentRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data(All_Grouped_CurrentRecruitment_for_plotting, 
                                                                                    Criteria, 
                                                                                    CurrentRecruitment_answers, 
                                                                                    CurrentRecruitment_colors, 
                                                                                    title_plot = title_plot_All_CurrentRecruitment, 
                                                                                    legend_position = "bottom")
All_Grouped_CurrentRecruitment_plot


## Future Recruitment
All_Data_FutureRecruitment <- rbind(
  pgrdata_FutureRecruitment,
  staffdata_FutureRecruitment,
  supportstaffdata_FutureRecruitment,
  academicdata_FutureRecruitment)

All_Split_FutureRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, All_Data_FutureRecruitment, FutureRecruitment_answers, FutureRecruitment_columns)
All_Grouped_FutureRecruitment_for_plotting <- regroup_all_data(All_Split_FutureRecruitment_for_plotting)


title_plot_All_FutureRecruitment <- paste ("Desired future recruitment criteria
(all researchers, N=",(as.numeric(sst_pgrdata$Total[sst_pgrdata$Question == "FutureRecruitment"])+
                                                                     as.numeric(sst_staffdata$Total[sst_staffdata$Question == "FutureRecruitment"])+
                                                                     as.numeric(sst_supportstaffdata$Total[sst_supportstaffdata$Question == "FutureRecruitment"])+
                                                                     as.numeric(sst_academicdata$Total[sst_academicdata$Question == "FutureRecruitment"])),
                                            ")" , sep="")


All_Grouped_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data_x_right(All_Grouped_FutureRecruitment_for_plotting, 
                                                                                    Criteria, 
                                                                                    FutureRecruitment_answers, 
                                                                                    FutureRecruitment_colors, 
                                                                                    title_plot = title_plot_All_FutureRecruitment)
All_Grouped_FutureRecruitment_plot

# combine
Combined_Recruitment_Doubleplot <- ggpubr::ggarrange(All_Grouped_CurrentRecruitment_plot,All_Grouped_FutureRecruitment_plot, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
Combined_Recruitment_Doubleplot
# ggsave("Figures/Round12_Single_AllDiv_Combined_Recruitment.png", width = 13, height = 7, bg = "white")





## combined plot for ORP
All_Data_But_Academic_CurrentRecruitment_forORP <- horizontal_stack_barplot_per_ORP(All_Data_But_Academic_CurrentRecruitment_for_plotting[All_Data_But_Academic_CurrentRecruitment_for_plotting$LabelIndiv == 'Open research practices',]
                                                                                    ,  Plotted_Div, Criteria,CurrentRecruitment_answers, CurrentRecruitment_colors, title_legend = NULL, title_plot = title_plot_Current_allbutacademic)
academicdata_CurrentRecruitment_forORP <- horizontal_stack_barplot_per_ORP(academicdata_CurrentRecruitment_for_plotting[academicdata_CurrentRecruitment_for_plotting$LabelIndiv == 'Open research practices',]
                                                                           , Plotted_Div, Criteria,CurrentRecruitment_answers, CurrentRecruitment_colors, title_legend = NULL, title_plot = title_plot_Current_academic)

All_Data_But_Academic_FutureRecruitment_forORP <- horizontal_stack_barplot_per_ORP(All_Data_But_Academic_FutureRecruitment_for_plotting[All_Data_But_Academic_FutureRecruitment_for_plotting$LabelIndiv == 'Open research practices',]
                                                                                   ,  Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_allbutacademic)
academicdata_FutureRecruitment_forORP <- horizontal_stack_barplot_per_ORP(academicdata_FutureRecruitment_for_plotting[academicdata_FutureRecruitment_for_plotting$LabelIndiv == 'Open research practices',]
                                                                          , Plotted_Div, Criteria,FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = title_plot_Future_academic)


doubleplotAcademicvsNonAc_CurrentRecruitment_forORP <- ggpubr::ggarrange(All_Data_But_Academic_CurrentRecruitment_forORP,
                                                                         academicdata_CurrentRecruitment_forORP,
                                                                         ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
doubleplotAcademicvsNonAc_CurrentRecruitment_forORP <- annotate_figure(doubleplotAcademicvsNonAc_CurrentRecruitment_forORP,
                                                                       top = text_grob("Perceived current recruitment criteria",
                                                                                       face = "bold", size = 14))
doubleplotAcademicvsNonAc_CurrentRecruitment_forORP



doubleplotAcademicvsNonAc_FutureRecruitment_forORP <- ggpubr::ggarrange(All_Data_But_Academic_FutureRecruitment_forORP,
                                                                        academicdata_FutureRecruitment_forORP,
                                                                        ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
doubleplotAcademicvsNonAc_FutureRecruitment_forORP <- annotate_figure(doubleplotAcademicvsNonAc_FutureRecruitment_forORP,
                                                                      top = text_grob("Desired future recruitment criteria",
                                                                                      face = "bold", size = 14))
doubleplotAcademicvsNonAc_FutureRecruitment_forORP



QuadrupleplotAcademicvsNonAc_FutureRecruitment_forORP <-  ggpubr::ggarrange(doubleplotAcademicvsNonAc_CurrentRecruitment_forORP,
                                                                            doubleplotAcademicvsNonAc_FutureRecruitment_forORP,
                                                                            ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
QuadrupleplotAcademicvsNonAc_FutureRecruitment_forORP

QuadrupleplotAcademicvsNonAc_FutureRecruitment_forORP <-  ggpubr::ggarrange(All_Data_But_Academic_CurrentRecruitment_forORP,
                                                                            All_Data_But_Academic_FutureRecruitment_forORP,
                                                                            academicdata_CurrentRecruitment_forORP,
                                                                            academicdata_FutureRecruitment_forORP,
                                                                            ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
QuadrupleplotAcademicvsNonAc_FutureRecruitment_forORP



#### sample size slightly off because in sample size script count 1 respondent for the question if answered any of the item
### a few didn't actually answer the ORP item
academicCurrent <- academicdata_CurrentRecruitment_for_plotting[academicdata_CurrentRecruitment_for_plotting$LabelIndiv == 'Open research practices',]
academicFuture <-academicdata_FutureRecruitment_for_plotting[academicdata_FutureRecruitment_for_plotting$LabelIndiv == 'Open research practices',]


academicCurrent %>% group_by(Div) %>% summarise(N = sum(n, na.rm=TRUE))
academicFuture %>% group_by(Div) %>% summarise(N = sum(n, na.rm=TRUE))

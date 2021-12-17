  #source("Rscripts/FormatData.R")
  #source("Rscripts/CurrentRecruitment.R")
  #source("Rscripts/FutureRecruitment.R")


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
#ggsave("Figures/Combined_Recruitment_Doubleplot.png", width = 13, height = 7, bg = "white")


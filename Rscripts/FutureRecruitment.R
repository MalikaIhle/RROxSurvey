
  #source("Rscripts/FormatData.R")

Criteria
FutureRecruitment_columns <- c(expr(FutureRecruitment_PubNub), expr(FutureRecruitment_PubPrestige), expr(FutureRecruitment_PubQual), expr(FutureRecruitment_Authorship),expr(FutureRecruitment_Citation),
                                expr(FutureRecruitment_Grant),expr(FutureRecruitment_Impact),expr(FutureRecruitment_Teaching),expr(FutureRecruitment_Supervision),expr(FutureRecruitment_Service),
                                expr(FutureRecruitment_Citizenship),expr(FutureRecruitment_Reputation),expr(FutureRecruitment_Collaboration),expr(FutureRecruitment_OpenResearch))
FutureRecruitment_answers <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")
FutureRecruitment_colors <- c("black", "#666666", "#FDE0DD",'#FA9FB5',"#F768A1",'#DD3497')

# create dataset for plotting per Divisions
pgrdata_FutureRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, pgrdata_FutureRecruitment, FutureRecruitment_answers, FutureRecruitment_columns)
staffdata_FutureRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, staffdata_FutureRecruitment, FutureRecruitment_answers, FutureRecruitment_columns)

# regroup data split per Division for overall plot
All_pgrdata_FutureRecruitment_for_plotting <- regroup_all_data(pgrdata_FutureRecruitment_for_plotting)
All_staffdata_FutureRecruitment_for_plotting <- regroup_all_data(staffdata_FutureRecruitment_for_plotting)

# Horizontal stacked barplot
All_pgrdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = "PGR students")
All_staffdata_FutureRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data(All_staffdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors, title_plot = "Researchers")

doubleplot_FutureRecruitment <- ggarrange(All_pgrdata_FutureRecruitment_plot, 
                                           All_staffdata_FutureRecruitment_plot, 
                                           ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_FutureRecruitment <- annotate_figure(doubleplot_FutureRecruitment, 
                                                 top = text_grob("Desired recruitment criteria", 
                                                                 face = "bold", size = 14))
#ggsave("Figures/FutureCriteria.png", width = 12, height = 6, bg = "white")



# circular plot per Division
## pgrdata_FutureRecruitment_plot <- circular_plot_function(pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, title_plot = 'Future Recruitment Criteria', FutureRecruitment_colors)

# plot regrouped data 
## All_pgrdata_FutureRecruitment_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_FutureRecruitment_for_plotting, Criteria, FutureRecruitment_answers, FutureRecruitment_colors)

# Horizontal stacked barplot per ORP
## pgrdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_FutureRecruitment_for_plotting, FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = "PGR students")
## staffdata_FutureRecruitment_perORP <- horizontal_stack_barplot_per_ORP(staffdata_FutureRecruitment_for_plotting, FutureRecruitment_answers, FutureRecruitment_colors, title_legend = NULL, title_plot = "Researchers")
## 
## doubleplot_FutureRecruitment <- ggarrange(pgrdata_FutureRecruitment_perORP, 
##                                   staffdata_FutureRecruitment_perORP, 
##                                   ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
## doubleplot_FutureRecruitment <- annotate_figure(doubleplot_FutureRecruitment, 
##                                         top = text_grob("FutureRecruitment of ORPs", 
##                                                         face = "bold", size = 14))
###ggsave("Figures/FutureRecruitment.png", width = 10, height = 13, bg = "white")

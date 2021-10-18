
  #source("Rscripts/FormatData.R")

Criteria
CurrentRecruitment_columns <- c(expr(CurrentRecruitment_PubNub), expr(CurrentRecruitment_PubPrestige), expr(CurrentRecruitment_PubQual), expr(CurrentRecruitment_Authorship),expr(CurrentRecruitment_Citation),
                      expr(CurrentRecruitment_Grant),expr(CurrentRecruitment_Impact),expr(CurrentRecruitment_Teaching),expr(CurrentRecruitment_Supervision),expr(CurrentRecruitment_Service),
                      expr(CurrentRecruitment_Citizenship),expr(CurrentRecruitment_Reputation),expr(CurrentRecruitment_Collaboration),expr(CurrentRecruitment_OpenResearch))
CurrentRecruitment_answers <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")
CurrentRecruitment_colors <- c("black", "#666666", "#FDE0DD",'#FA9FB5',"#F768A1",'#DD3497')

# create dataset for plotting per Divisions
pgrdata_CurrentRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, pgrdata_CurrentRecruitment, CurrentRecruitment_answers, CurrentRecruitment_columns)
staffdata_CurrentRecruitment_for_plotting <- prepare_data_for_plotting(Criteria, staffdata_CurrentRecruitment, CurrentRecruitment_answers, CurrentRecruitment_columns)

# regroup data split per Division for overall plot
All_pgrdata_CurrentRecruitment_for_plotting <- regroup_all_data(pgrdata_CurrentRecruitment_for_plotting)
All_staffdata_CurrentRecruitment_for_plotting <- regroup_all_data(staffdata_CurrentRecruitment_for_plotting)

# Horizontal stacked barplot
All_pgrdata_CurrentRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors, title_plot = "PGR students")
All_staffdata_CurrentRecruitment_plot <- horizontal_stacked_barplot_on_regrouped_data(All_staffdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors, title_plot = "Researchers")

doubleplot_CurrentRecruitment <- ggarrange(All_pgrdata_CurrentRecruitment_plot, 
                                           All_staffdata_CurrentRecruitment_plot, 
                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_CurrentRecruitment <- annotate_figure(doubleplot_CurrentRecruitment, 
                                        top = text_grob("Perceived current recruitment criteria", 
                                                        face = "bold", size = 14))
#ggsave("Figures/CurrentCriteria.png", width = 12, height = 6, bg = "white")



# circular plot per Division
## pgrdata_CurrentRecruitment_plot <- circular_plot_function(pgrdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, title_plot = 'Current Recruitment Criteria', CurrentRecruitment_colors)

# plot regrouped data 
## All_pgrdata_CurrentRecruitment_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_CurrentRecruitment_for_plotting, Criteria, CurrentRecruitment_answers, CurrentRecruitment_colors)

# Horizontal stacked barplot per ORP
## pgrdata_CurrentRecruitment_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_CurrentRecruitment_for_plotting, CurrentRecruitment_answers, CurrentRecruitment_colors, title_legend = NULL, title_plot = "PGR students")
## staffdata_CurrentRecruitment_perORP <- horizontal_stack_barplot_per_ORP(staffdata_CurrentRecruitment_for_plotting, CurrentRecruitment_answers, CurrentRecruitment_colors, title_legend = NULL, title_plot = "Researchers")
## 
## doubleplot_CurrentRecruitment <- ggarrange(pgrdata_CurrentRecruitment_perORP, 
##                                   staffdata_CurrentRecruitment_perORP, 
##                                   ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
## doubleplot_CurrentRecruitment <- annotate_figure(doubleplot_CurrentRecruitment, 
##                                         top = text_grob("CurrentRecruitment of ORPs", 
##                                                         face = "bold", size = 14))
###ggsave("Figures/CurrentRecruitment.png", width = 10, height = 13, bg = "white")


  #source("Rscripts/FormatData.R")

# Variables to define
Measures
Barriers_columns <- c(expr(Barriers_OA), expr(Barriers_Data), expr(Barriers_Code), expr(Barriers_Materials),expr(Barriers_Preprint),expr(Barriers_Prereg),expr(Barriers_RegRep))
Barriers_answers <- c("None", "Other", "Policy",  "Incentives","Norms" , "Training", "Infrastructure", "NotSure","NA")

# create dataset for plotting per Divisions
pgrdata_Barriers_for_plotting <- prepare_barriers_data_for_plotting(pgrdata_Barriers, Barriers_answers, Barriers_columns )
staffdata_Barriers_for_plotting <- prepare_barriers_data_for_plotting(staffdata_Barriers, Barriers_answers, Barriers_columns )

# regroup data split per Division for overall plot
All_pgrdata_Barriers_for_plotting <- regroup_all_barriers_data(pgrdata_Barriers_for_plotting)
All_staffdata_Barriers_for_plotting <- regroup_all_barriers_data(staffdata_Barriers_for_plotting)

# circular plot per Division
## pgrdata_Barriers_plot <- barriers_circular_plot_function(pgrdata_Barriers_for_plotting)

# plot regrouped data 
## All_pgrdata_Barriers_plot <- stacked_barplot_on_barriers_regrouped_data(All_pgrdata_Barriers_for_plotting, Measures, Barriers_answers)

# Horizontal stacked barplot per ORP # are those corrects?
pgrdata_Barriers_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Barriers_for_plotting, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = "PGR students")
staffdata_Barriers_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Barriers_for_plotting, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = "Researchers")

doubleplot_Barriers <- ggarrange(pgrdata_Barriers_perORP, 
                                  staffdata_Barriers_perORP, 
                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Barriers <- annotate_figure(doubleplot_Barriers, 
                                        top = text_grob("Barriers of ORPs", 
                                                        face = "bold", size = 14))
#ggsave("Figures/Barriers-per-ORP.png", width = 10, height = 9, bg = "white")


  
  
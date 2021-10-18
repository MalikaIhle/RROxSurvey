
  #source("Rscripts/FormatData.R")

Measures
Effect_columns <- c(expr(Effect_OA), expr(Effect_Data), expr(Effect_Code), expr(Effect_Materials),expr(Effect_Preprint),expr(Effect_Prereg),expr(Effect_RegRep))
Effect_answers <- c("Beneficial", "Neutral (neither detrimental nor beneficial)", "Detrimental","Not sure", "Not applicable")
Effect_colors <- c("black", "#666666", "#D95F02", "#E6AB02", "#1B9E77")

# create dataset for plotting per Divisions
pgrdata_Effect_for_plotting <- prepare_data_for_plotting(Measures, pgrdata_Effect, Effect_answers, Effect_columns)
staffdata_Effect_for_plotting <- prepare_data_for_plotting(Measures, staffdata_Effect, Effect_answers, Effect_columns)

# regroup data split per Division for overall plot
All_pgrdata_Effect_for_plotting <- regroup_all_data(pgrdata_Effect_for_plotting)
All_staffdata_Effect_for_plotting <- regroup_all_data(staffdata_Effect_for_plotting)

# circular plot per Division
## pgrdata_Effect_plot <- circular_plot_function(pgrdata_Effect_for_plotting, Measures, Effect_answers, title_plot = 'Effect', Effect_colors)

# plot regrouped data 
## All_pgrdata_Effect_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Effect_for_plotting, Measures, Effect_answers, Effect_colors)

# Horizontal stacked barplot per ORP
pgrdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Effect_for_plotting, Effect_answers, Effect_colors, title_legend = NULL, title_plot = "PGR students")
staffdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Effect_for_plotting, Effect_answers, Effect_colors, title_legend = NULL, title_plot = "Researchers")

doubleplot_Effect <- ggarrange(pgrdata_Effect_perORP, 
                                  staffdata_Effect_perORP, 
                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Effect <- annotate_figure(doubleplot_Effect, 
                                        top = text_grob("Effect of ORPs", 
                                                        face = "bold", size = 14))
#ggsave("Figures/Effect-per-ORP.png", width = 10, height = 9, bg = "white")

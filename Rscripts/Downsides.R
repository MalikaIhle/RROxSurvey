
  #source("Rscripts/FormatData.R")

Measures
Downsides_columns <- c(expr(Downsides_OA), expr(Downsides_Data), expr(Downsides_Code), expr(Downsides_Materials),expr(Downsides_Preprint),expr(Downsides_Prereg),expr(Downsides_RegRep))
Downsides_answers <- c("No", "Yes",  "Not sure",  "Not applicable" )
Downsides_colors <- c("black", "#666666", "#D95F02", "#1B9E77")

# create dataset for plotting per Divisions
pgrdata_Downsides_for_plotting <- prepare_data_for_plotting(Measures, pgrdata_Downsides, Downsides_answers, Downsides_columns)
staffdata_Downsides_for_plotting <- prepare_data_for_plotting(Measures, staffdata_Downsides, Downsides_answers, Downsides_columns)

# regroup data split per Division for overall plot
All_pgrdata_Downsides_for_plotting <- regroup_all_data(pgrdata_Downsides_for_plotting)
All_staffdata_Downsides_for_plotting <- regroup_all_data(staffdata_Downsides_for_plotting)

# circular plot per Division
## pgrdata_Downsides_plot <- circular_plot_function(pgrdata_Downsides_for_plotting, Measures, Downsides_answers, title_plot = 'Downsides', Downsides_colors)

# plot regrouped data 
## All_pgrdata_Downsides_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Downsides_for_plotting, Measures, Downsides_answers, Downsides_colors)

# Horizontal stacked barplot per ORP
pgrdata_Downsides_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Downsides_for_plotting, Downsides_answers, Downsides_colors, title_legend = NULL, title_plot = "PGR students")
staffdata_Downsides_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Downsides_for_plotting, Downsides_answers, Downsides_colors, title_legend = NULL, title_plot = "Researchers")

doubleplot_Downsides <- ggarrange(pgrdata_Downsides_perORP, 
                               staffdata_Downsides_perORP, 
                               ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Downsides <- annotate_figure(doubleplot_Downsides, 
                                     top = text_grob("Downsides of ORPs", 
                                                     face = "bold", size = 14))
#ggsave("Figures/Downsides-per-ORP.png", width = 10, height = 9, bg = "white")

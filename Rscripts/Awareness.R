################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

  #source("Rscripts/FormatData.R")

# Variables to define
Measures
Awareness_columns <- c(expr(Awareness_OA), expr(Awareness_Data), expr(Awareness_Code), expr(Awareness_Materials),expr(Awareness_Preprint),expr(Awareness_Prereg),expr(Awareness_RegRep))
Awareness_answers <- c("Practicing myself", "Accessing / using only", "Aware only",  "Not aware / not sure if applicable",  "Not applicable" )
Awareness_colors <- rev(c("#ABDDA4", "#FFFFBF", "#FDAE61", "#D7191C", "black"))

# create dataset for plotting per Divisions
pgrdata_Awareness_for_plotting <- prepare_data_for_plotting(Measures, pgrdata_Awareness, Awareness_answers, Awareness_columns)
staffdata_Awareness_for_plotting <- prepare_data_for_plotting(Measures, staffdata_Awareness, Awareness_answers, Awareness_columns)

# regroup data split per Division for overall plot
All_pgrdata_Awareness_for_plotting <- regroup_all_data(pgrdata_Awareness_for_plotting)

# circular plot per Division
## pgrdata_Awareness_plot <- circular_plot_function(pgrdata_Awareness_for_plotting, Measures, Awareness_answers, title_plot = 'Awareness', Awareness_colors)

# plot regrouped data 
## All_pgrdata_Awareness_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Awareness_for_plotting, Measures, Awareness_answers, Awareness_colors)

# Horizontal stacked barplot per ORP
pgrdata_Awareness_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Awareness_for_plotting, Awareness_answers, Awareness_colors, title_legend = NULL, title_plot = "PGR students")
staffdata_Awareness_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Awareness_for_plotting, Awareness_answers, Awareness_colors, title_legend = NULL, title_plot = "Researchers")

doubleplot_Awareness <- ggarrange(pgrdata_Awareness_perORP, 
                                  staffdata_Awareness_perORP, 
                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Awareness <- annotate_figure(doubleplot_Awareness, 
                                        top = text_grob("Awareness of ORPs", 
                                                        face = "bold", size = 14))
  #ggsave("Figures/Awareness-per-ORP.png", width = 10, height = 9, bg = "white")

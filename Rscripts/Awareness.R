
  #source("Rscripts/FormatData.R")

# Variables to define
Measures
Awareness_columns <- c(expr(Awareness_OA), expr(Awareness_Data), expr(Awareness_Code), expr(Awareness_Materials),expr(Awareness_Preprint),expr(Awareness_Prereg),expr(Awareness_RegRep))
Awareness_answers <- c("Practicing myself", "Accessing / using only", "Aware only",  "Not aware / not sure if applicable",  "Not applicable" )
Awareness_colors <- rev(c("#ABDDA4", "#FFFFBF", "#FDAE61", "#D7191C", "black"))

# create dataset for plotting per Divisions
pgrdata_Awareness_for_plotting <- prepare_data_for_plotting(Measures, pgrdata_Awareness, Awareness_answers, Awareness_columns)
allstaffdata_Awareness_for_plotting <- prepare_data_for_plotting(Measures, allstaffdata_Awareness, Awareness_answers, Awareness_columns)
staffdata_Awareness_for_plotting <- prepare_data_for_plotting(Measures, staffdata_Awareness, Awareness_answers, Awareness_columns)
supportstaffdata_Awareness_for_plotting <- prepare_data_for_plotting(Measures, supportstaffdata_Awareness, Awareness_answers, Awareness_columns)
academicdata_Awareness_for_plotting <- prepare_data_for_plotting(Measures, academicdata_Awareness, Awareness_answers, Awareness_columns)

# regroup data split per Division for overall plot
All_pgrdata_Awareness_for_plotting <- regroup_all_data(pgrdata_Awareness_for_plotting)
All_allstaffdata_Awareness_for_plotting <- regroup_all_data(allstaffdata_Awareness_for_plotting)
All_staffdata_Awareness_for_plotting <- regroup_all_data(staffdata_Awareness_for_plotting)
All_supportstaffdata_Awareness_for_plotting <- regroup_all_data(supportstaffdata_Awareness_for_plotting)
All_academicdata_Awareness_for_plotting <- regroup_all_data(academicdata_Awareness_for_plotting)

# circular plot per Division
## pgrdata_Awareness_plot <- circular_plot_function(pgrdata_Awareness_for_plotting, Measures, Awareness_answers, title_plot = 'Awareness', Awareness_colors)

# plot regrouped data 
## All_pgrdata_Awareness_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Awareness_for_plotting, Measures, Awareness_answers, Awareness_colors)

# Horizontal stacked barplot per ORP
title_plot_pgr <- paste ("PGR students (N=",sst_pgrdata$Total[sst_pgrdata$Question == "Awareness"], ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sst_staffdata$Total[sst_staffdata$Question == "Awareness"], ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Awareness"], ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sst_academicdata$Total[sst_academicdata$Question == "Awareness"], ")" , sep="")

pgrdata_Awareness_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Awareness_for_plotting, Measures, Awareness_answers, Awareness_colors, title_legend = NULL, title_plot = title_plot_pgr)
allstaffdata_Awareness_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_Awareness_for_plotting, Measures,Awareness_answers, Awareness_colors, title_legend = NULL, title_plot = "Researchers")
staffdata_Awareness_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Awareness_for_plotting, Measures,Awareness_answers, Awareness_colors, title_legend = NULL, title_plot = title_plot_staff)
supportstaffdata_Awareness_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_Awareness_for_plotting,Measures, Awareness_answers, Awareness_colors, title_legend = NULL, title_plot = title_plot_supportstaff)
academicdata_Awareness_perORP <- horizontal_stack_barplot_per_ORP(academicdata_Awareness_for_plotting, Measures, Awareness_answers, Awareness_colors, title_legend = NULL, title_plot = title_plot_academic)


#doubleplot_Awareness <- ggpubr::ggarrange(pgrdata_Awareness_perORP, 
#                                  allstaffdata_Awareness_perORP, 
#                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
#doubleplot_Awareness <- annotate_figure(doubleplot_Awareness, 
#                                        top = text_grob("Awareness of ORPs", 
#                                                        face = "bold", size = 14))
#  #ggsave("Figures/Awareness-per-ORP.png", width = 10, height = 9, bg = "white")

quadrupleplot_Awareness <- ggpubr::ggarrange(pgrdata_Awareness_perORP, 
                                     staffdata_Awareness_perORP, 
                                     supportstaffdata_Awareness_perORP,
                                     academicdata_Awareness_perORP,
                                     ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_Awareness <- annotate_figure(quadrupleplot_Awareness, 
                                        top = text_grob("Awareness of ORPs", 
                                                        face = "bold", size = 14))
quadrupleplot_Awareness
#ggsave("Figures/quadrupleplot_Awareness-per-ORP.png", width = 10, height = 10, bg = "white")


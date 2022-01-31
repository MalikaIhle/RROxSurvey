
  #source("Rscripts/FormatData.R")

Measures
Effect_columns <- c(expr(Effect_OA), expr(Effect_Data), expr(Effect_Code), expr(Effect_Materials),expr(Effect_Preprint),expr(Effect_Prereg),expr(Effect_RegRep))
Effect_answers <- c("Beneficial", "Neutral (neither detrimental nor beneficial)", "Detrimental","Not sure", "Not applicable")
Effect_colors <- c("black", "#666666", "#D95F02", "#E6AB02", "#1B9E77")

# create dataset for plotting per Divisions
pgrdata_Effect_for_plotting <- prepare_data_for_plotting(Measures, pgrdata_Effect, Effect_answers, Effect_columns)
allstaffdata_Effect_for_plotting <- prepare_data_for_plotting(Measures, allstaffdata_Effect, Effect_answers, Effect_columns)
staffdata_Effect_for_plotting <- prepare_data_for_plotting(Measures, staffdata_Effect, Effect_answers, Effect_columns)
supportstaffdata_Effect_for_plotting <- prepare_data_for_plotting(Measures, supportstaffdata_Effect, Effect_answers, Effect_columns)
academicdata_Effect_for_plotting <- prepare_data_for_plotting(Measures, academicdata_Effect, Effect_answers, Effect_columns)

# regroup data split per Division for overall plot
All_pgrdata_Effect_for_plotting <- regroup_all_data(pgrdata_Effect_for_plotting)
All_allstaffdata_Effect_for_plotting <- regroup_all_data(allstaffdata_Effect_for_plotting)
All_staffdata_Effect_for_plotting <- regroup_all_data(staffdata_Effect_for_plotting)
All_supportstaffdata_Effect_for_plotting <- regroup_all_data(supportstaffdata_Effect_for_plotting)
All_academicstaffdata_Effect_for_plotting <- regroup_all_data(academicdata_Effect_for_plotting)

# circular plot per Division
## pgrdata_Effect_plot <- circular_plot_function(pgrdata_Effect_for_plotting, Measures, Effect_answers, title_plot = 'Effect', Effect_colors)

# plot regrouped data 
## All_pgrdata_Effect_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Effect_for_plotting, Measures, Effect_answers, Effect_colors)

# Horizontal stacked barplot per ORP
title_plot_pgr <- paste ("PGR students (N=",sst_pgrdata$Total[sst_pgrdata$Question == "Effect"], ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sst_staffdata$Total[sst_staffdata$Question == "Effect"], ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Effect"], ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sst_academicdata$Total[sst_academicdata$Question == "Effect"], ")" , sep="")

pgrdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Effect_for_plotting, Measures, Effect_answers, Effect_colors, title_legend = NULL, title_plot = title_plot_pgr)
allstaffdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_Effect_for_plotting, Measures, Effect_answers, Effect_colors, title_legend = NULL, title_plot = "Researchers")
staffdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Effect_for_plotting, Measures, Effect_answers, Effect_colors, title_legend = NULL, title_plot = title_plot_staff)
supportstaffdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_Effect_for_plotting, Measures, Effect_answers, Effect_colors, title_legend = NULL, title_plot =title_plot_supportstaff)
academicdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(academicdata_Effect_for_plotting, Measures, Effect_answers, Effect_colors, title_legend = NULL, title_plot = title_plot_academic)

# doubleplot_Effect <- ggpubr::ggarrange(pgrdata_Effect_perORP, 
#                                   allstaffdata_Effect_perORP, 
#                                   ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
# doubleplot_Effect <- annotate_figure(doubleplot_Effect, 
#                                         top = text_grob("Effect of ORPs", 
#                                                         face = "bold", size = 14))
# ggsave("Figures/Effect-per-ORP.png", width = 10, height = 9, bg = "white")

quadrupleplot_Effect <- ggpubr::ggarrange(pgrdata_Effect_perORP, 
                                     staffdata_Effect_perORP, 
                                     supportstaffdata_Effect_perORP,
                                     academicdata_Effect_perORP,
                                     ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_Effect <- annotate_figure(quadrupleplot_Effect, 
                                           top = text_grob("Overall effect of widespread adoption of ORPs", 
                                                           face = "bold", size = 14))
quadrupleplot_Effect
#ggsave("Figures/quadrupleplot_Effect-per-ORP.png", width = 10, height = 10, bg = "white")



## all data pooled across Div and target pop
All_Data_Effect <- rbind(
  pgrdata_Effect,
  staffdata_Effect,
  supportstaffdata_Effect,
  academicdata_Effect)

All_Split_Effect_for_plotting <- prepare_data_for_plotting(Measures, All_Data_Effect, Effect_answers, Effect_columns)
All_Grouped_Effect_for_plotting <- regroup_all_data(All_Split_Effect_for_plotting)

title_plot_All_Effect <- paste ("Effect of ORPs
(all researchers, N=",(as.numeric(sst_pgrdata$Total[sst_pgrdata$Question == "Effect"])+
                         as.numeric(sst_staffdata$Total[sst_staffdata$Question == "Effect"])+
                         as.numeric(sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Effect"])+
                         as.numeric(sst_academicdata$Total[sst_academicdata$Question == "Effect"])),
                         ")" , sep="")


All_Grouped_Effect_plot <- horizontal_stacked_barplot_on_regrouped_data(All_Grouped_Effect_for_plotting, 
                                                                           Measures, 
                                                                           Effect_answers, 
                                                                           Effect_colors, 
                                                                           title_plot = title_plot_All_Effect, 
                                                                           legend_position = "bottom")
All_Grouped_Effect_plot


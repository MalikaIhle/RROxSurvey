
  #source("Rscripts/FormatData.R")

Trainings <- c('Open Access', 'Data Management', 'FAIR Data','Ethics','Open Code', 'Open Materials', 'Licences', 'Preprint', 'Preregistration', 'Recruitement')
Training_columns <- c(expr(Training_OA), expr(Training_DMP), expr(Training_FAIR), expr(Training_Ethics),
                      expr(Training_Code),expr(Training_Materials),expr(Training_Licences), 
                       expr(Training_Preprint), expr(Training_Prereg), expr(Training_Recruitement))
Training_answers <- c("Written guidance and workshop-led training", "Written guidance only", "No guidance wanted", "No guidance needed",  "Not sure",  "Not applicable" )
Training_colors <- c("black", "#666666", "#ABDDA4", "#FFFFBF", '#FDAE61', '#D7191C')

# create dataset for plotting per Divisions
pgrdata_Training_for_plotting <- prepare_data_for_plotting(Trainings, pgrdata_Training, Training_answers, Training_columns)
allstaffdata_Training_for_plotting <- prepare_data_for_plotting(Trainings, allstaffdata_Training, Training_answers, Training_columns)
staffdata_Training_for_plotting <- prepare_data_for_plotting(Trainings, staffdata_Training, Training_answers, Training_columns)
supportstaffdata_Training_for_plotting <- prepare_data_for_plotting(Trainings, supportstaffdata_Training, Training_answers, Training_columns)
academicdata_Training_for_plotting <- prepare_data_for_plotting(Trainings, academicdata_Training, Training_answers, Training_columns)

# regroup data split per Division for overall plot
All_pgrdata_Training_for_plotting <- regroup_all_data(pgrdata_Training_for_plotting)
All_allstaffdata_Training_for_plotting <- regroup_all_data(allstaffdata_Training_for_plotting)
All_staffdata_Training_for_plotting <- regroup_all_data(staffdata_Training_for_plotting)
All_supportstaffdata_Training_for_plotting <- regroup_all_data(supportstaffdata_Training_for_plotting)
All_academicdata_Training_for_plotting <- regroup_all_data(academicdata_Training_for_plotting)

# circular plot per Division
## pgrdata_Training_plot <- circular_plot_function(pgrdata_Training_for_plotting, Trainings, Training_answers, title_plot = 'Training', Training_colors)

# plot regrouped data 
## All_pgrdata_Training_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Training_for_plotting, Trainings, Training_answers, Training_colors)

# Horizontal stacked barplot per ORP
title_plot_pgr <- paste ("PGR students (N=",sst_pgrdata$Total[sst_pgrdata$Question == "Training"], ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sst_staffdata$Total[sst_staffdata$Question == "Training"], ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Training"], ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sst_academicdata$Total[sst_academicdata$Question == "Training"], ")" , sep="")

pgrdata_Training_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Training_for_plotting, Training_answers, Training_colors, title_legend = NULL, title_plot = title_plot_pgr)
allstaffdata_Training_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_Training_for_plotting, Training_answers, Training_colors, title_legend = NULL, title_plot = "Researchers")
staffdata_Training_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Training_for_plotting, Training_answers, Training_colors, title_legend = NULL, title_plot = title_plot_staff)
supportstaffdata_Training_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_Training_for_plotting, Training_answers, Training_colors, title_legend = NULL, title_plot = title_plot_supportstaff)
academicdata_Training_perORP <- horizontal_stack_barplot_per_ORP(academicdata_Training_for_plotting, Training_answers, Training_colors, title_legend = NULL, title_plot =title_plot_academic)


# doubleplot_Training <- ggpubr::ggarrange(pgrdata_Training_perORP, 
#                                allstaffdata_Training_perORP, 
#                                ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
# doubleplot_Training <- annotate_figure(doubleplot_Training, 
#                                      top = text_grob("Training needs", 
#                                                      face = "bold", size = 14))
#ggsave("Figures/Training.png", width = 10, height = 13, bg = "white")



quadrupleplot_Training <- ggpubr::ggarrange(pgrdata_Training_perORP, 
                                     staffdata_Training_perORP, 
                                     supportstaffdata_Training_perORP,
                                     academicdata_Training_perORP,
                                     ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_Training <- annotate_figure(quadrupleplot_Training, 
                                           top = text_grob("Training needs", 
                                                           face = "bold", size = 14))
quadrupleplot_Training
#ggsave("Figures/quadrupleplot_Training.png", width = 10, height = 13, bg = "white")


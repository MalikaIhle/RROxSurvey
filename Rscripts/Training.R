
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
alldata_Training_for_plotting <- prepare_data_for_plotting(Trainings, data_Training, Training_answers, Training_columns)

# regroup data split per Division for overall plot
All_pgrdata_Training_for_plotting <- regroup_all_data(pgrdata_Training_for_plotting)
All_allstaffdata_Training_for_plotting <- regroup_all_data(allstaffdata_Training_for_plotting)
All_staffdata_Training_for_plotting <- regroup_all_data(staffdata_Training_for_plotting)
All_supportstaffdata_Training_for_plotting <- regroup_all_data(supportstaffdata_Training_for_plotting)
All_academicdata_Training_for_plotting <- regroup_all_data(academicdata_Training_for_plotting)

# circular plot per Division
## pgrdata_Training_plot <- circular_plot_function(pgrdata_Training_for_plotting, Trainings, Training_answers, title_plot = 'Training', Training_colors)

# Horizontal stack bar plot on regrouped data (all div)

title_plot_pgr_regrouped <- paste ("PGR students (N=",sst_pgrdata$Total[sst_pgrdata$Question == "Training"], ")" , sep="")
title_plot_allstaff_regrouped <- paste ("Researchers (N=",sum(as.numeric(sst_staffdata$Total[sst_staffdata$Question == "Training"]),
                                                    as.numeric(sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Training"]),
                                                    as.numeric(sst_academicdata$Total[sst_academicdata$Question == "Training"])), ")" , sep="")
title_plot_staff_regrouped <- paste ("Research staff (N=",sst_staffdata$Total[sst_staffdata$Question == "Training"], ")" , sep="")
title_plot_supportstaff_regrouped <- paste ("Research support staff (N=",sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Training"], ")" , sep="")
title_plot_academic_regrouped <- paste ("Academics (N=",sst_academicdata$Total[sst_academicdata$Question == "Training"], ")" , sep="")

# plot regrouped data 
temp <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_Training_for_plotting, Trainings, Training_answers, Training_colors, title_plot = title_plot_pgr_regrouped, legend_position = "bottom")
shared_legend <- extract_legend(temp)

All_pgrdata_Training_plot <- horizontal_stacked_barplot_on_regrouped_data(All_pgrdata_Training_for_plotting, Trainings, Training_answers, Training_colors, title_plot = title_plot_pgr_regrouped, legend_position = "none")
All_allstaffdata_Training_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_allstaffdata_Training_for_plotting, Trainings, Training_answers, Training_colors, title_plot = title_plot_allstaff_regrouped)
All_staffdata_Training_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_staffdata_Training_for_plotting, Trainings, Training_answers, Training_colors, title_plot = title_plot_staff_regrouped)
All_supportstaffdata_Training_plot <- horizontal_stacked_barplot_on_regrouped_data_without_axis_text(All_supportstaffdata_Training_for_plotting, Trainings, Training_answers, Training_colors, title_plot = title_plot_supportstaff_regrouped)
All_academicdata_Training_plot <- horizontal_stacked_barplot_on_regrouped_data_x_right(All_academicdata_Training_for_plotting, Trainings, Training_answers, Training_colors, title_plot = title_plot_academic_regrouped)


doubleplot_All_Training <- egg::ggarrange(All_pgrdata_Training_plot, 
                                                    All_allstaffdata_Training_plot, 
                                                    nrow=1)

doubleplot_All_Training_with_legend <- ggpubr::ggarrange(doubleplot_All_Training, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/

doubleplot_All_Training_with_legend <- annotate_figure(doubleplot_All_Training_with_legend, top = text_grob("Training needs", face = "bold", size = 14))

doubleplot_All_Training_with_legend
# ggsave("Figures/Round12_Double_AllDiv_Training.png", width = 15, height = 10, bg = "white")


quadrupleplot_All_Training <- egg::ggarrange(All_pgrdata_Training_plot, 
                                            All_staffdata_Training_plot, 
                                            All_supportstaffdata_Training_plot,
                                            All_academicdata_Training_plot,
                                            nrow=1)

quadrupleplot_All_Training_with_legend <- ggpubr::ggarrange(quadrupleplot_All_Training, shared_legend, nrow = 2, heights = c(10, 1)) # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r/
quadrupleplot_All_Training_with_legend <- annotate_figure(quadrupleplot_All_Training_with_legend, top = text_grob("Training needs", face = "bold", size = 14))
quadrupleplot_All_Training_with_legend
# ggsave("Figures/Round12_Quadruple_AllDiv_Training.png", width = 12, height = 4, bg = "white")


## all data pooled across Div and target pop
All_Data_Training <- rbind(
  pgrdata_Training,
  staffdata_Training,
  supportstaffdata_Training,
  academicdata_Training)

All_Split_Training_for_plotting <- prepare_data_for_plotting(Trainings, All_Data_Training, Training_answers, Training_columns)
All_Grouped_Training_for_plotting <- regroup_all_data(All_Split_Training_for_plotting)

title_plot_All_Training <- paste ("Training of ORPs
(all researchers, N=",(as.numeric(sst_pgrdata$Total[sst_pgrdata$Question == "Training"])+
                         as.numeric(sst_staffdata$Total[sst_staffdata$Question == "Training"])+
                         as.numeric(sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Training"])+
                         as.numeric(sst_academicdata$Total[sst_academicdata$Question == "Training"])),
                         ")" , sep="")


All_Grouped_Training_plot <- horizontal_stacked_barplot_on_regrouped_data(All_Grouped_Training_for_plotting, 
                                                                          Trainings, 
                                                                          Training_answers, 
                                                                          Training_colors, 
                                                                          title_plot = title_plot_All_Training, 
                                                                          legend_position = "bottom")
All_Grouped_Training_plot
#ggsave("Figures/Round12_Single_Training.png", width = 10, height = 4, bg = "white")



# Horizontal stacked barplot per ORP, Div split
Plotted_Div <- c("MSD", "MPLS","SSD", "Hum")
title_plot_pgr <- paste ("PGR students (N=",sum(as.numeric(sst_pgrdata[sst_pgrdata$Question == "Training", Plotted_Div ])), ")" , sep="")
title_plot_allstaff <- paste ("Researchers (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "Training", Plotted_Div ]),
                                                    as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "Training", Plotted_Div ]),
                                                    as.numeric(sst_academicdata[sst_academicdata$Question == "Training", Plotted_Div ])), ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "Training", Plotted_Div ])), ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sum(as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "Training", Plotted_Div ])), ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sum(as.numeric(sst_academicdata[sst_academicdata$Question == "Training", Plotted_Div ])), ")" , sep="")
title_plot_Current_alldata <- paste ("PGR students and all researchers combined (N=",sum(as.numeric(sst_pgrdata[sst_pgrdata$Question == "Training", Plotted_Div ]),
                                                                                         as.numeric(sst_staffdata[sst_staffdata$Question == "Training", Plotted_Div ]),
                                                                                         as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "Training", Plotted_Div ]),
                                                                                         as.numeric(sst_academicdata[sst_academicdata$Question == "Training", Plotted_Div ])),")" , sep="")


pgrdata_Training_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Training_for_plotting, Plotted_Div, Trainings, Training_answers, Training_colors, title_legend = NULL, title_plot = title_plot_pgr)
allstaffdata_Training_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_Training_for_plotting, Plotted_Div, Trainings,Training_answers, Training_colors, title_legend = NULL, title_plot = title_plot_allstaff)
staffdata_Training_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Training_for_plotting, Plotted_Div, Trainings,Training_answers, Training_colors, title_legend = NULL, title_plot = title_plot_staff)
supportstaffdata_Training_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_Training_for_plotting, Plotted_Div, Trainings,Training_answers, Training_colors, title_legend = NULL, title_plot = title_plot_supportstaff)
academicdata_Training_perORP <- horizontal_stack_barplot_per_ORP(academicdata_Training_for_plotting, Plotted_Div, Trainings,Training_answers, Training_colors, title_legend = NULL, title_plot =title_plot_academic)

alldata_Training_perORP <- horizontal_stack_barplot_per_ORP(alldata_Training_for_plotting,  Plotted_Div, Trainings,Training_answers, Training_colors, title_legend = NULL, title_plot = title_plot_Current_alldata)
alldata_Training_perORP
# ggsave("Figures/Round12_Single_splitDiv_Training.png", width = 8, height = 20, bg = "white")


doubleplot_Training <- ggpubr::ggarrange(pgrdata_Training_perORP,
                               allstaffdata_Training_perORP,
                               ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Training <- annotate_figure(doubleplot_Training,
                                     top = text_grob("Training needs",
                                                     face = "bold", size = 14))
doubleplot_Training
# ggsave("Figures/Round12_Double_SplitDiv_Training.png", width = 10, height = 15, bg = "white")



quadrupleplot_Training <- ggpubr::ggarrange(pgrdata_Training_perORP, 
                                     staffdata_Training_perORP, 
                                     supportstaffdata_Training_perORP,
                                     academicdata_Training_perORP,
                                     ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_Training <- annotate_figure(quadrupleplot_Training, 
                                           top = text_grob("Training needs", 
                                                           face = "bold", size = 14))
quadrupleplot_Training
# ggsave("Figures/Round12_Quadruple_SplitDiv_Training.png", width = 10, height = 13, bg = "white")



  #source("Rscripts/FormatData.R")

Measures
Downsides_columns <- c(expr(Downsides_OA), expr(Downsides_Data), expr(Downsides_Code), expr(Downsides_Materials),expr(Downsides_Preprint),expr(Downsides_Prereg),expr(Downsides_RegRep))
Downsides_answers <- c("No", "Yes",  "Not sure",  "Not applicable" )
Downsides_colors <- c("black", "#666666", "#D95F02", "#1B9E77")


# create dataset for plotting per Divisions
pgrdata_Downsides_for_plotting <- prepare_data_for_plotting(Measures, pgrdata_Downsides, Downsides_answers, Downsides_columns)
allstaffdata_Downsides_for_plotting <- prepare_data_for_plotting(Measures, allstaffdata_Downsides, Downsides_answers, Downsides_columns)
staffdata_Downsides_for_plotting <- prepare_data_for_plotting(Measures, staffdata_Downsides, Downsides_answers, Downsides_columns)
supportstaffdata_Downsides_for_plotting <- prepare_data_for_plotting(Measures, supportstaffdata_Downsides, Downsides_answers, Downsides_columns)
academicdata_Downsides_for_plotting <- prepare_data_for_plotting(Measures, academicdata_Downsides, Downsides_answers, Downsides_columns)

# regroup data split per Division for overall plot
All_pgrdata_Downsides_for_plotting <- regroup_all_data(pgrdata_Downsides_for_plotting)
All_allstaffdata_Downsides_for_plotting <- regroup_all_data(allstaffdata_Downsides_for_plotting)
All_staffdata_Downsides_for_plotting <- regroup_all_data(staffdata_Downsides_for_plotting)
All_supportstaffdata_Downsides_for_plotting <- regroup_all_data(supportstaffdata_Downsides_for_plotting)
All_academicstaffdata_Downsides_for_plotting <- regroup_all_data(academicdata_Downsides_for_plotting)

# circular plot per Division
## pgrdata_Downsides_plot <- circular_plot_function(pgrdata_Downsides_for_plotting, Measures, Downsides_answers, title_plot = 'Downsides', Downsides_colors)

# plot regrouped data 
## All_pgrdata_Downsides_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Downsides_for_plotting, Measures, Downsides_answers, Downsides_colors)

# Horizontal stacked barplot per ORP
Plotted_Div <- c("MSD", "MPLS","SSD", "Hum")
title_plot_pgr <- paste ("PGR students (N=",sum(as.numeric(sst_pgrdata[sst_pgrdata$Question == "Downsides", Plotted_Div ])), ")" , sep="")
title_plot_allstaff <- paste ("Researchers (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "Downsides", Plotted_Div ]),
                                                    as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "Downsides", Plotted_Div ]),
                                                    as.numeric(sst_academicdata[sst_academicdata$Question == "Downsides", Plotted_Div ])), ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "Downsides", Plotted_Div ])), ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sum(as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "Downsides", Plotted_Div ])), ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sum(as.numeric(sst_academicdata[sst_academicdata$Question == "Downsides", Plotted_Div ])), ")" , sep="")

pgrdata_Downsides_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Downsides_for_plotting, Plotted_Div, Measures, Downsides_answers, Downsides_colors, title_legend = NULL, title_plot = title_plot_pgr)
allstaffdata_Downsides_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_Downsides_for_plotting, Plotted_Div, Measures, Downsides_answers, Downsides_colors, title_legend = NULL, title_plot = title_plot_allstaff)
staffdata_Downsides_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Downsides_for_plotting, Plotted_Div, Measures, Downsides_answers, Downsides_colors, title_legend = NULL, title_plot = title_plot_staff)
supportstaffdata_Downsides_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_Downsides_for_plotting, Plotted_Div, Measures, Downsides_answers, Downsides_colors, title_legend = NULL, title_plot =title_plot_supportstaff)
academicdata_Downsides_perORP <- horizontal_stack_barplot_per_ORP(academicdata_Downsides_for_plotting,Plotted_Div, Measures, Downsides_answers, Downsides_colors, title_legend = NULL, title_plot = title_plot_academic)

doubleplot_Downsides <- ggpubr::ggarrange(pgrdata_Downsides_perORP,
                                  allstaffdata_Downsides_perORP,
                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Downsides <- annotate_figure(doubleplot_Downsides,
                                        top = text_grob("Downsides of ORPs",
                                                        face = "bold", size = 14))
doubleplot_Downsides
# ggsave("Figures/Round12_Double_Downsides-per-ORP.png", width = 10, height = 9, bg = "white")

quadrupleplot_Downsides <- ggpubr::ggarrange(pgrdata_Downsides_perORP, 
                                  staffdata_Downsides_perORP, 
                                  supportstaffdata_Downsides_perORP,
                                  academicdata_Downsides_perORP,
                                  ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_Downsides <- annotate_figure(quadrupleplot_Downsides, 
                                        top = text_grob("Any downsides of ORPs", 
                                                        face = "bold", size = 14))
quadrupleplot_Downsides
# ggsave("Figures/Round12_Quadruple_Downsides-per-ORP.png", width = 10, height = 10, bg = "white")


## all data pooled across Div and target pop
All_Data_Downsides <- rbind(
  pgrdata_Downsides,
  staffdata_Downsides,
  supportstaffdata_Downsides,
  academicdata_Downsides)

All_Split_Downsides_for_plotting <- prepare_data_for_plotting(Measures, All_Data_Downsides, Downsides_answers, Downsides_columns)
All_Grouped_Downsides_for_plotting <- regroup_all_data(All_Split_Downsides_for_plotting)

title_plot_All_Downsides <- paste ("Downsides of ORPs
(all researchers, N=",(as.numeric(sst_pgrdata$Total[sst_pgrdata$Question == "Downsides"])+
                         as.numeric(sst_staffdata$Total[sst_staffdata$Question == "Downsides"])+
                         as.numeric(sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Downsides"])+
                         as.numeric(sst_academicdata$Total[sst_academicdata$Question == "Downsides"])),
                         ")" , sep="")


All_Grouped_Downsides_plot <- horizontal_stacked_barplot_on_regrouped_data(All_Grouped_Downsides_for_plotting, 
                                                                        Measures, 
                                                                        Downsides_answers, 
                                                                        Downsides_colors, 
                                                                        title_plot = title_plot_All_Downsides, 
                                                                        legend_position = "bottom")
All_Grouped_Downsides_plot
# ggsave("Figures/Round12_Single_Downsides-per-ORP.png", width = 10, height = 4, bg = "white")


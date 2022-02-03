
  #source("Rscripts/FormatData.R")

# Variables to define
Measures
Barriers_columns <- c(expr(Barriers_OA), expr(Barriers_Data), expr(Barriers_Code), expr(Barriers_Materials),expr(Barriers_Preprint),expr(Barriers_Prereg),expr(Barriers_RegRep))
Barriers_answers <- c("None", "Other", "Policy",  "Incentives","Norms" , "Training", "Infrastructure", "NotSure","NA")
Barriers_colors <- c("black", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186")

# create dataset for plotting per Divisions
pgrdata_Barriers_for_plotting <- prepare_barriers_data_for_plotting(pgrdata_Barriers, Barriers_answers, Barriers_columns )
allstaffdata_Barriers_for_plotting <- prepare_barriers_data_for_plotting(allstaffdata_Barriers, Barriers_answers, Barriers_columns )
staffdata_Barriers_for_plotting <- prepare_barriers_data_for_plotting(staffdata_Barriers, Barriers_answers, Barriers_columns )
supportstaffdata_Barriers_for_plotting <- prepare_barriers_data_for_plotting(supportstaffdata_Barriers, Barriers_answers, Barriers_columns )
academicdata_Barriers_for_plotting <- prepare_barriers_data_for_plotting(academicdata_Barriers, Barriers_answers, Barriers_columns )

# regroup data split per Division for overall plot
All_pgrdata_Barriers_for_plotting <- regroup_all_barriers_data(pgrdata_Barriers_for_plotting)
All_allstaffdata_Barriers_for_plotting <- regroup_all_barriers_data(allstaffdata_Barriers_for_plotting)
All_staffdata_Barriers_for_plotting <- regroup_all_barriers_data(staffdata_Barriers_for_plotting)
All_supportstaffdata_Barriers_for_plotting <- regroup_all_barriers_data(supportstaffdata_Barriers_for_plotting)
All_academicdata_Barriers_for_plotting <- regroup_all_barriers_data(academicdata_Barriers_for_plotting)

# circular plot per Division
## pgrdata_Barriers_plot <- barriers_circular_plot_function(pgrdata_Barriers_for_plotting)

# plot regrouped data 
#All_pgrdata_Barriers_plot <- stacked_barplot_on_barriers_regrouped_data(All_pgrdata_Barriers_for_plotting, Measures, Barriers_answers)
# dodged_barplot_on_barriers_regrouped_data(All_pgrdata_Barriers_for_plotting, Measures, Barriers_answers)

Plotted_Div <- c("MSD", "MPLS","SSD", "Hum")
title_plot_pgr <- paste ("PGR students (N=",sum(as.numeric(sst_pgrdata[sst_pgrdata$Question == "Barriers", Plotted_Div ])), ")" , sep="")
title_plot_allstaff <- paste ("Researchers (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "Barriers", Plotted_Div ]),
                                                    as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "Barriers", Plotted_Div ]),
                                                    as.numeric(sst_academicdata[sst_academicdata$Question == "Barriers", Plotted_Div ])), ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "Barriers", Plotted_Div ])), ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sum(as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "Barriers", Plotted_Div ])), ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sum(as.numeric(sst_academicdata[sst_academicdata$Question == "Barriers", Plotted_Div ])), ")" , sep="")


all_barrier_plotting_data <- rbind(All_pgrdata_Barriers_for_plotting,All_staffdata_Barriers_for_plotting, All_supportstaffdata_Barriers_for_plotting, All_academicdata_Barriers_for_plotting)
max(all_barrier_plotting_data$perc)

All_pgrdata_Barriers_dodgeplot <- horizontal_dodged_barplot_on_barriers_regrouped_data(All_pgrdata_Barriers_for_plotting,  Measures, Barriers_answers,title_plot_pgr , 60)
All_allstaffdata_Barriers_dodgeplot <-horizontal_dodged_barplot_on_barriers_regrouped_data(All_allstaffdata_Barriers_for_plotting,  Measures, Barriers_answers, title_plot_allstaff, 60)
All_staffdata_Barriers_dodgeplot <-horizontal_dodged_barplot_on_barriers_regrouped_data(All_staffdata_Barriers_for_plotting,  Measures, Barriers_answers, title_plot_staff, 60)
All_supportstaffdata_Barriers_dodgeplot <-horizontal_dodged_barplot_on_barriers_regrouped_data(All_supportstaffdata_Barriers_for_plotting, Measures, Barriers_answers, title_plot_supportstaff, 60)
All_academicdata_Barriers_dodgeplot <-horizontal_dodged_barplot_on_barriers_regrouped_data(All_academicdata_Barriers_for_plotting,  Measures, Barriers_answers, title_plot_academic, 60)


doubleplot_Barriers <- ggpubr::ggarrange(All_pgrdata_Barriers_dodgeplot,
                                         All_allstaffdata_Barriers_dodgeplot,
                                          ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Barriers <- annotate_figure(doubleplot_Barriers,
                                        top = text_grob("Barriers of ORPs",
                                                        face = "bold", size = 14))
doubleplot_Barriers
# ggsave("Figures/Round12_Double_AllDiv_Barriers-per-ORP.png", width = 10, height = 9, bg = "white")


quadrupleplot_All_Barriers <- ggarrange(All_pgrdata_Barriers_dodgeplot, All_staffdata_Barriers_dodgeplot, All_supportstaffdata_Barriers_dodgeplot, All_academicdata_Barriers_dodgeplot,
          ncol = 4, nrow = 1, common.legend = TRUE, legend="right")
quadrupleplot_All_Barriers <- annotate_figure(quadrupleplot_All_Barriers, 
                                           top = text_grob("Barriers to adoption of ORPs (non-mutually exclusive)", 
                                                           face = "bold", size = 14))
quadrupleplot_All_Barriers
#ggsave("Figures/Round12_Quadruple_AllDiv_Barriers.png", width = 13, height = 9, bg = "white")

# plot merged accross Div and target groups
All_Data_Barriers <- rbind(
  pgrdata_Barriers,
  staffdata_Barriers,
  supportstaffdata_Barriers,
  academicdata_Barriers)

All_Split_Barriers_for_plotting <- prepare_barriers_data_for_plotting(All_Data_Barriers, Barriers_answers, Barriers_columns)
All_Grouped_Barriers_for_plotting <- regroup_all_barriers_data(All_Split_Barriers_for_plotting)

max(All_Grouped_Barriers_for_plotting$perc)

title_plot_All_Barriers <- paste ("Barriers to adoption of ORPs
(all researchers, N=",(as.numeric(sst_pgrdata$Total[sst_pgrdata$Question == "Barriers"])+
                         as.numeric(sst_staffdata$Total[sst_staffdata$Question == "Barriers"])+
                         as.numeric(sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Barriers"])+
                         as.numeric(sst_academicdata$Total[sst_academicdata$Question == "Barriers"])),
                         ")" , sep="")

All_Grouped_Barriers_dodgeplot <- horizontal_dodged_barplot_on_barriers_regrouped_data(All_Grouped_Barriers_for_plotting, Measures, Barriers_answers,title_plot_All_Barriers , 50)
All_Grouped_Barriers_dodgeplot
#ggsave("Figures/Round12_Single_AllDiv_Barriers.png", width = 5, height = 7, bg = "white")


# Horizontal stacked barplot per ORP 
# pgrdata_Barriers_perORP <- barriers_horizontal_stack_barplot_per_ORP(pgrdata_Barriers_for_plotting, Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = title_plot_pgr, 600)
# staffdata_Barriers_perORP <- barriers_horizontal_stack_barplot_per_ORP(staffdata_Barriers_for_plotting, Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = title_plot_staff, 600)
# supportstaffdata_Barriers_perORP <- barriers_horizontal_stack_barplot_per_ORP(supportstaffdata_Barriers_for_plotting, Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = title_plot_supportstaff,600)
# academicdata_Barriers_perORP <- barriers_horizontal_stack_barplot_per_ORP(academicdata_Barriers_for_plotting, Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = title_plot_academic, 600)

# Horizontal dodged barplot per ORP 
temp <- rbind(pgrdata_Barriers_for_plotting,staffdata_Barriers_for_plotting, supportstaffdata_Barriers_for_plotting, academicdata_Barriers_for_plotting)
max(temp$perc, na.rm=TRUE)

pgrdata_Barriers_perORP <- barriers_horizontal_dodge_barplot_per_ORP(pgrdata_Barriers_for_plotting, Plotted_Div, Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = title_plot_pgr, 100)
staffdata_Barriers_perORP <- barriers_horizontal_dodge_barplot_per_ORP(staffdata_Barriers_for_plotting, Plotted_Div, Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = title_plot_staff, 100)
supportstaffdata_Barriers_perORP <- barriers_horizontal_dodge_barplot_per_ORP(supportstaffdata_Barriers_for_plotting,Plotted_Div,  Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = title_plot_supportstaff, 100)
academicdata_Barriers_perORP <- barriers_horizontal_dodge_barplot_per_ORP(academicdata_Barriers_for_plotting, Plotted_Div, Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = title_plot_academic, 100)


quadrupleplot_Barriers <- ggpubr::ggarrange(pgrdata_Barriers_perORP,
                                            staffdata_Barriers_perORP,
                                            supportstaffdata_Barriers_perORP,
                                            academicdata_Barriers_perORP,
                                  ncol=4, nrow=1, common.legend = TRUE, legend="right")
quadrupleplot_Barriers <- annotate_figure(quadrupleplot_Barriers,
                                        top = text_grob("Barriers to adoption of ORPs",
                                                        face = "bold", size = 14))
quadrupleplot_Barriers
#ggsave("Figures/Round12_Quadruple_Barriers.png", width = 10, height = 25, bg = "white")


  
  
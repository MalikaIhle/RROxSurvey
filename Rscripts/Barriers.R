
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


title_plot_pgr <- paste ("PGR students (N=",sst_pgrdata$Total[sst_pgrdata$Question == "Barriers"], ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sst_staffdata$Total[sst_staffdata$Question == "Barriers"], ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sst_supportstaffdata$Total[sst_supportstaffdata$Question == "Barriers"], ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sst_academicdata$Total[sst_academicdata$Question == "Barriers"], ")" , sep="")

# plot regrouped data 
#All_pgrdata_Barriers_plot <- stacked_barplot_on_barriers_regrouped_data(All_pgrdata_Barriers_for_plotting, Measures, Barriers_answers)
# dodged_barplot_on_barriers_regrouped_data(All_pgrdata_Barriers_for_plotting, Measures, Barriers_answers)
all_barrier_plotting_data <- rbind(All_pgrdata_Barriers_for_plotting,All_staffdata_Barriers_for_plotting, All_supportstaffdata_Barriers_for_plotting, All_academicdata_Barriers_for_plotting)
max(all_barrier_plotting_data$perc)

All_pgrdata_Barriers_dodgeplot <- horizontal_dodged_barplot_on_barriers_regrouped_data(All_pgrdata_Barriers_for_plotting, Measures, Barriers_answers,title_plot_pgr )
All_staffdata_Barriers_dodgeplot <-horizontal_dodged_barplot_on_barriers_regrouped_data(All_staffdata_Barriers_for_plotting, Measures, Barriers_answers, title_plot_staff)
All_supportstaffdata_Barriers_dodgeplot <-horizontal_dodged_barplot_on_barriers_regrouped_data(All_supportstaffdata_Barriers_for_plotting, Measures, Barriers_answers, title_plot_supportstaff)
All_academicdata_Barriers_dodgeplot <-horizontal_dodged_barplot_on_barriers_regrouped_data(All_academicdata_Barriers_for_plotting, Measures, Barriers_answers, title_plot_academic)

quadrupleplot_All_Barriers <- ggarrange(All_pgrdata_Barriers_dodgeplot, All_staffdata_Barriers_dodgeplot, All_supportstaffdata_Barriers_dodgeplot, All_academicdata_Barriers_dodgeplot,
          ncol = 4, nrow = 1, common.legend = TRUE, legend="right")
quadrupleplot_All_Barriers <- annotate_figure(quadrupleplot_All_Barriers, 
                                           top = text_grob("Barriers to adoption of ORPs (non-mutually exclusive)", 
                                                           face = "bold", size = 14))
quadrupleplot_All_Barriers
#ggsave("Figures/quadrupleplot_All_Barriers.png", width = 13, height = 9, bg = "white")


# Horizontal stacked barplot per ORP # are those corrects?
barriers_horizontal_stack_barplot_per_ORP(pgrdata_Barriers_for_plotting, Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = "PGR students")


#pgrdata_Barriers_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Barriers_for_plotting, Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = "PGR students")
#allstaffdata_Barriers_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_Barriers_for_plotting, Measures, Barriers_answers, Barriers_colors, title_legend = NULL, title_plot = "Researchers")

# doubleplot_Barriers <- ggpubr::ggarrange(pgrdata_Barriers_perORP, 
#                                   allstaffdata_Barriers_perORP, 
#                                   ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
# doubleplot_Barriers <- annotate_figure(doubleplot_Barriers, 
#                                         top = text_grob("Barriers of ORPs", 
#                                                         face = "bold", size = 14))
#ggsave("Figures/Barriers-per-ORP.png", width = 10, height = 9, bg = "white")


  
  
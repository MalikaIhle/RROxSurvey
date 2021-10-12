################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
################################################

#source("Rscripts/FormatData.R")

# Variables to define
Measures
Awareness_columns <- c(expr(Awareness_OA), expr(Awareness_Data), expr(Awareness_Code), expr(Awareness_Materials),expr(Awareness_Preprint),expr(Awareness_Prereg),expr(Awareness_RegRep))
Awareness_answers <- c("Practicing myself", "Accessing / using only", "Aware only",  "Not aware / not sure if applicable",  "Not applicable" )
answers_colors <- rev(c("#ABDDA4", "#FFFFBF", "#FDAE61", "#D7191C", "black"))
#title_plot <- 'Awareness'
title_plot <- 'PGR students awareness of ORPs'
skeleton <- create_skeleton(Measures, Divisions, Awareness_answers, Awareness_columns) # create skeleton of all possible answers

# create PGR dataset for plotting per Divisions -----
pgrdata_Awareness <- pgrdata[, c(grep("Div", colnames(pgrdata)), grep(pattern="^Awareness", x=colnames(pgrdata)))]
summaryitems_pgrdata_Awareness <- bind_summaries_items(Measures, pgrdata_Awareness, Awareness_columns)
pgrdata_Awareness <- merge(skeleton, summaryitems_pgrdata_Awareness, by = "ID", all.x = TRUE) # merge summary items to skeleton

# create Staff dataset for plotting per Divisions -----
staffdata_Awareness <- staffdata[, c(grep("Div", colnames(staffdata)), grep(pattern="^Awareness", x=colnames(staffdata)))]
summaryitems_staffdata_Awareness <- bind_summaries_items(Measures, staffdata_Awareness, Awareness_columns)
staffdata_Awareness <- merge(skeleton, summaryitems_staffdata_Awareness, by = "ID", all.x = TRUE) # merge summary items to skeleton


rm(skeleton, summaryitems_pgrdata_Awareness, summaryitems_staffdata_Awareness, Awareness_columns)


# plot per Division -----
## pgrdata_Awareness_plot <- circular_plot_function(pgrdata_Awareness, Measures, Awareness_answers, title_plot, answers_colors)
## pgrdata_Awareness_plot
## ggsave(pgrdata_Awareness_plot, file=here("Figures/pgrdata_Awareness2.png"), width=10, height=8)

# regroup data split per Division for overall plot -----
All_pgrdata_Awareness <- regroup_all_data(pgrdata_Awareness)

# plot regrouped data  -----
## All_pgrdata_Awareness_plot <- stacked_barplot_on_regrouped_data(All_pgrdata_Awareness, Measures, Awareness_answers, answers_colors)
## All_pgrdata_Awareness_plot
## ggsave(All_pgrdata_Support_plot, file=here("Figures/All_pgrdata_Awareness_plot.png"), width=10, height=8)


# Horizontal stacked barplot per ORP
pgrdata_Awareness_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Awareness, Awareness_answers, answers_colors, title_legend = NULL, title_plot = "PGR students")
pgrdata_Awareness_perORP
#ggsave("Figures/example-plot-Awareness-per-ORP.png", width = 5, height = 9, bg = "white")

staffdata_Awareness_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Awareness, Awareness_answers, answers_colors, title_legend = NULL, title_plot = "Researchers")
staffdata_Awareness_perORP

doubleplot_Awareness <- ggarrange(pgrdata_Awareness_perORP, staffdata_Awareness_perORP, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(doubleplot_Awareness,top = text_grob("Awareness of ORPs", face = "bold", size = 14))

doubleplot_Awareness <- ggarrange(pgrdata_Awareness_perORP, 
                                  staffdata_Awareness_perORP, 
                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Awareness <- annotate_figure(doubleplot_Awareness, top = text_grob("Awareness of ORPs", face = "bold", size = 14))
#ggsave("Figures/Awareness-per-ORP.png", width = 10, height = 9, bg = "white")

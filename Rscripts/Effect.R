
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
Plotted_Div <- c("MSD", "MPLS","SSD", "Hum")
title_plot_pgr <- paste ("PGR students (N=",sum(as.numeric(sst_pgrdata[sst_pgrdata$Question == "Effect", Plotted_Div ])), ")" , sep="")
title_plot_allstaff <- paste ("Researchers (N=",sum(as.numeric(sst_allstaffdata[sst_allstaffdata$Question == "Effect", Plotted_Div ])), ")" , sep="")
title_plot_staff <- paste ("Research staff (N=",sum(as.numeric(sst_staffdata[sst_staffdata$Question == "Effect", Plotted_Div ])), ")" , sep="")
title_plot_supportstaff <- paste ("Research support staff (N=",sum(as.numeric(sst_supportstaffdata[sst_supportstaffdata$Question == "Effect", Plotted_Div ])), ")" , sep="")
title_plot_academic <- paste ("Academics (N=",sum(as.numeric(sst_academicdata[sst_academicdata$Question == "Effect", Plotted_Div ])), ")" , sep="")

pgrdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(pgrdata_Effect_for_plotting, Plotted_Div, Measures, Effect_answers, Effect_colors, title_legend = NULL, title_plot = title_plot_pgr)
allstaffdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(allstaffdata_Effect_for_plotting, Plotted_Div, Measures, Effect_answers, Effect_colors, title_legend = NULL, title_plot = title_plot_allstaff)
staffdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(staffdata_Effect_for_plotting, Plotted_Div, Measures, Effect_answers, Effect_colors, title_legend = NULL, title_plot = title_plot_staff)
supportstaffdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(supportstaffdata_Effect_for_plotting, Plotted_Div, Measures, Effect_answers, Effect_colors, title_legend = NULL, title_plot =title_plot_supportstaff)
academicdata_Effect_perORP <- horizontal_stack_barplot_per_ORP(academicdata_Effect_for_plotting, Plotted_Div, Measures, Effect_answers, Effect_colors, title_legend = NULL, title_plot = title_plot_academic)

doubleplot_Effect <- ggpubr::ggarrange(pgrdata_Effect_perORP,
                                  allstaffdata_Effect_perORP,
                                  ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
doubleplot_Effect <- annotate_figure(doubleplot_Effect,
                                        top = text_grob("Effect of ORPs",
                                                        face = "bold", size = 14))
doubleplot_Effect
#ggsave(here::here("Figures", "Round12_Double_Effect-per-ORP.png"), width = 10, height = 9, bg = "white")

quadrupleplot_Effect <- ggpubr::ggarrange(pgrdata_Effect_perORP, 
                                     staffdata_Effect_perORP, 
                                     supportstaffdata_Effect_perORP,
                                     academicdata_Effect_perORP,
                                     ncol=4, nrow=1, common.legend = TRUE, legend="bottom")
quadrupleplot_Effect <- annotate_figure(quadrupleplot_Effect, 
                                           top = text_grob("Overall effect of widespread adoption of ORPs", 
                                                           face = "bold", size = 14))
quadrupleplot_Effect
#ggsave(here::here("Figures", "Round12_Quadruple_Effect-per-ORP.png"), width = 10, height = 10, bg = "white")



## all data pooled across Div and target pop
All_Data_Effect <- rbind(
  pgrdata_Effect,
  staffdata_Effect,
  supportstaffdata_Effect,
  academicdata_Effect)

All_Split_Effect_for_plotting <- prepare_data_for_plotting(Measures, All_Data_Effect, Effect_answers, Effect_columns)
All_Grouped_Effect_for_plotting <- regroup_all_data(All_Split_Effect_for_plotting)

title_plot_All_Effect <- paste ("Effect of ORPs
(all researchers, N=",(as.numeric(sst_data$Total[sst_data$Question == "Effect"])),")" , sep="")


All_Grouped_Effect_plot <- horizontal_stacked_barplot_on_regrouped_data(All_Grouped_Effect_for_plotting, 
                                                                           Measures, 
                                                                           Effect_answers, 
                                                                           Effect_colors, 
                                                                           title_plot = title_plot_All_Effect, 
                                                                           legend_position = "bottom")
All_Grouped_Effect_plot
#ggsave(here::here("Figures", "Round12_Single_Effect-per-ORP.png"), width = 10, height = 4, bg = "white")



## plot for academics only, all ORPs combined.
academicdata_Effect_acrossORP <- academicdata_Effect[academicdata_Effect$Div %in% Plotted_Div,]
academicdata_Effect_acrossORP <- academicdata_Effect_acrossORP[complete.cases(academicdata_Effect_acrossORP), ]

academicdata_Effect_acrossORP <- data.frame(rbind(cbind(academicdata_Effect_acrossORP$Div, academicdata_Effect_acrossORP$Effect_OA), 
                                                     cbind(academicdata_Effect_acrossORP$Div, academicdata_Effect_acrossORP$Effect_Data), 
                                                     cbind(academicdata_Effect_acrossORP$Div, academicdata_Effect_acrossORP$Effect_Code), 
                                                     cbind(academicdata_Effect_acrossORP$Div, academicdata_Effect_acrossORP$Effect_Materials),
                                                     cbind(academicdata_Effect_acrossORP$Div, academicdata_Effect_acrossORP$Effect_Preprint),
                                                     cbind(academicdata_Effect_acrossORP$Div, academicdata_Effect_acrossORP$Effect_Prereg), 
                                                     cbind(academicdata_Effect_acrossORP$Div, academicdata_Effect_acrossORP$Effect_RegRep)))
colnames(academicdata_Effect_acrossORP) <- c("Div", "Answer")

academicdata_Effect_acrossORP_for_plotting <- academicdata_Effect_acrossORP  %>% group_by(Div,Answer) %>% summarise (n = n()) %>% 
  mutate(perc = n / sum(n) * 100 ) 
academicdata_Effect_acrossORP_for_plotting$ID <- paste(academicdata_Effect_acrossORP_for_plotting$Div, academicdata_Effect_acrossORP_for_plotting$Answer)
academicdata_Effect_acrossORP_for_plotting$Div <- factor(academicdata_Effect_acrossORP_for_plotting$Div, levels = rev(Divisions))
academicdata_Effect_acrossORP_for_plotting$Answer = factor(academicdata_Effect_acrossORP_for_plotting$Answer, levels = Effect_answers)

Nacademic_Effect_perDiv <- academicdata_Effect_acrossORP_for_plotting %>% group_by(Div) %>% summarise(N = sum(n, na.rm=TRUE)/7) # divided by 7 because all responses for all 7 measures were pooled
Nacademic_Effect <- sum(Nacademic_Effect_perDiv$N)

title_plot <- paste('Effect of widespread adoption averaged accross all ORPs
Academics (N = ', Nacademic_Effect,")" , sep="")

academicdata_Effect_acrossORP_for_plotting %>% 
  ggplot() +
  geom_bar(aes(x = Div, y = perc, fill = Answer), stat = "identity", position = "fill") +
  scale_fill_manual(values = (Effect_colors),
                    breaks = rev(Effect_answers), 
                    labels = rev(Effect_answers), 
                    drop = FALSE) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="right", legend.title = element_blank()) +
  labs(x = "", y = "")+
  guides(fill=guide_legend())+
  ggtitle(title_plot) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))

#ggsave(here::here("Figures", "Round12_Single_Effect_Academic_accross_ORP.png"), width = 10, height = 3, bg = "white")



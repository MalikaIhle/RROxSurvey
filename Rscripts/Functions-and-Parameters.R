# items to judges or categories

Measures <- c('Open Access', 'Open Data', 'Open Code', 'Open Materials', 'Preprint', 'Preregistration', 'Registered Report')
Measures_short <- c('OA', 'Data', 'Code', 'Materials', 'Preprint', 'Prereg', 'RegRep')

Criteria <- c('Number of publications','Prestige of publication outlet','Quality of publications', 'Authorship role', 'Citations', 'Grant support', 
              'Impact','Teaching', 'Supervision, mentoring', 'Service to the profession','Citizenship','Reputation',
              'Collaboration network','Open research practices')
Criteria_short <- c("PubNub","PubPrestige","PubQual","Authorship","Citation","Grant","Impact", "Teaching","Supervision","Service","Citizenship",
                    "Reputation","Collaboration","OpenResearch")

Divisions <- c("MSD", "MPLS","SSD", "Hum", "ContEd", "GLAM", "College")

#Questions <- c("Awareness","Effect", "Barriers", "Downsides", "CurrentRecruitment", "FutureRecruitment", "Training", "Support")
Questions <- c(expr(Awareness),expr(Effect), expr(Barriers), expr(Downsides), expr(CurrentRecruitment), 
               expr(FutureRecruitment), expr(Training), expr(Support))

# functions

## Format raw data
clean_qualtrics_data <- function(data, surveyindex){
  
  ## change column names according to survey index
  data <- data[-c(1, 2), -which(names(data) %in% c("DistributionChannel","UserLanguage"))] 
  colnames(data) <- surveyindex$VariableName[surveyindex$QuestionCode == colnames(data)] 
  data[data == ""] <- NA
  
  ## Subset data to records with filled out Consent, Affiliation, and Role: the 3 mandatory questions
  #nrow(data)
  data <- subset(data[!is.na(data$Consent) & !is.na(data$DivCol) & !is.na(data$Role),] )
  #nrow(data)
  
  ## Consent T/F
  data$Consent <- data$Consent == "I consent to participating in the survey and to processing of the information I provide as outlined above."
  
  # Affiliation
  {
    ## College TRUE FALSE
    data$College <- data$DivCol == "College-only staff"
    #table(data$College)
    
    ## Division
    data$Div <- data$DivCol
    data$Div[data$Div == "College-only staff"] <- data$ColDiv[data$Div == "College-only staff"] # replacing College only staff affiliation to their Div of affinity
    data$Div[is.na(data$Div)] <- 'College' # removing college only staff who didn't say which Division their field of research was closed to...
    
    data$Div[data$Div == "Social Sciences Division"] <- "SSD"
    data$Div[data$Div == "Humanities Division"] <- "Hum"
    data$Div[data$Div == "Department for Continuing Education"] <- "ContEd"
    data$Div[data$Div == "Mathematical, Physical, and Life Sciences Division"] <- "MPLS"
    data$Div[data$Div == "Medical Sciences Division"] <- "MSD"
    data$Div[data$Div == "Gardens, Libraries and Museums"] <- "GLAM"
    #table(data$Div)
    
    ## Department
    data$Dept <- data$Dept1 
    data$Dept[!is.na(data$Dept2)] <- data$Dept2[!is.na(data$Dept2)]
    data$Dept[!is.na(data$Dept3)] <- data$Dept3[!is.na(data$Dept3)]
    data$Dept[!is.na(data$Dept4)] <- data$Dept4[!is.na(data$Dept4)]
    data$Dept[!is.na(data$Dept5)] <- data$Dept5[!is.na(data$Dept5)]
    data$Dept[!is.na(data$Dept6)] <- data$Dept6[!is.na(data$Dept6)]
    data$Dept[!is.na(data$Dept7)] <- data$Dept7[!is.na(data$Dept7)]
    data$Dept[!is.na(data$Dept8)] <- data$Dept8[!is.na(data$Dept8)]
    data$Dept[!is.na(data$Dept9)] <- data$Dept9[!is.na(data$Dept9)]
    data$Dept[!is.na(data$Dept10)] <- data$Dept10[!is.na(data$Dept10)]
    #table(data$Dept)
    
    # one department was renamed
    data$Dept[!is.na(data$Dept) & str_detect(data$Dept, "School of Interdisciplinary Area Studies")] <- "School of Global Area Studies" 
    
    
    ## Other Department
    #table(data$OtherDept)
    data[!is.na(data$OtherDept),c('OtherDept', 'Dept', 'Div')]
    
    ### recoding of Dept for otherdept actually in the list
    data$Dept[!is.na(data$OtherDept) & (data$OtherDept == "Wellcome Centre for Human Genetics" |
                                          data$OtherDept == "Experimental Medicine"|
                                          data$OtherDept == "NDM Experimental Medicine"|
                                          data$OtherDept == "Nuffield department of medicine"|
                                          data$OtherDept == "Division of Structural Biology"|
                                          data$OtherDept == "Nuffield Department of Experimental Medicine")] <- "Nuffield Department of Clinical Medicine"
    
    data$Dept[!is.na(data$OtherDept) & str_detect(data$OtherDept, "Oxford Internet Institute")] <- "Oxford Internet Institute"
    
    data[!is.na(data$Dept) & data$Dept == "Other",]
    
    # clean up long longer useful column
    unwanted_colnames <- c("DivCol", "ColDiv", names(data[, grep(pattern="Dept[0-9]+", colnames(data))]))
    data <- data[, -which(names(data) %in% unwanted_colnames)] 
    rm(unwanted_colnames)
  }
  
  # Role
  {
    #table(data$Role) 
    data$StudentStaff <- data$Role == "Student on a postgraduate research programme"
    data$StudentStaff[data$StudentStaff == TRUE] <- "Student"
    data$StudentStaff[data$StudentStaff == FALSE] <- "Staff"
  }
  
  # Years of Experience
  data$Duration <- as.numeric(data$Duration)
  
  return(data)
  
}

subset_columns_by_pattern <- function(data, pattern){
  data[, c(grep("Div", colnames(data)), grep(pattern=pattern, x=colnames(data)))]
}

## Calculate sample sizes
sample_size_perQ <- function(data, Question){
  
  # example to test function sample_size_perQ
  # data <- pgrdata_Awareness
  # Question <- "Awareness"
  
  ss <- data[rowSums(!is.na(data)) > 1, ] %>% 
    group_by(Div) %>% 
    summarise({{Question}} := n()) # https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr
  return(ss)
}

## Prepare data (all but barriers) for plotting
create_skeleton <- function(Question, answers, columns){ # needed for the legend in case some values were not represented
  Div <- rep(Divisions, each = length(columns)*length(answers)) 
  LabelIndiv <- rep(Question, each = length(answers), times = length(Divisions)) 
  Indiv <-paste(Div, LabelIndiv, sep ="_") 
  Answer <- rep(answers, times= length(Divisions)*length(columns)) 
  ID <- paste(Indiv, Answer, sep="_") 
  skeleton <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
}

summarise_item <-  function(data, item, name_item){  # item is the name of the columns e.g. Awareness_OA given as an expression (not a string)
  
  # example to test function summarise_item
  # data <- pgrdata_Awareness
  # columns <- c(expr(Awareness_OA), expr(Awareness_Data), expr(Awareness_Code), expr(Awareness_Materials),expr(Awareness_Preprint),expr(Awareness_Prereg),expr(Awareness_RegRep))
  # Question <- Measures
  # i <- 1
  # item <- columns[[i]]
  # name_item <- Question[i]
  # summarise_item(data, columns[[i]], Question[i])
  
  data2 <-  data[!is.na(data[,as.character(item)]), c("Div", as.character(item))]  %>%  # select 1 column (item) when non NA (e.g. 'Awareness_OA')
    group_by(Div,{{item}}) %>%  # the double curly bracket read item as e.g. Awareness_OA; ie. it groups by the type of answer (e.g. 'Accessing / using only')
    summarise (n = n()) %>% 
    mutate(perc = n / sum(n) * 100 ) 
  data2$ID = paste(paste(data2$Div, name_item, sep="_"), unlist(data2[,as.character(item)]), sep ="_") # use the full name of the item (e.g. "Open Access")
  return(data2[,c('ID', 'n', 'perc')])
  
}

bind_summaries_items <- function(Question, data, columns){
  
  # example to test function summarise_item
  # data <- pgrdata_Awareness
  # columns <- c(expr(Awareness_OA), expr(Awareness_Data), expr(Awareness_Code), expr(Awareness_Materials),expr(Awareness_Preprint),expr(Awareness_Prereg),expr(Awareness_RegRep))
  # Question <- Measures
  # i <- 1
  # item <- columns[[i]]
  # name_item <- Question[i]
  # summarize_item(data, item, name_item)
  # summarise_item(data, columns[[i]], Question[i])
  
  summaryitems <- vector(mode= "list", length = length(Question))
  for (i in 1:length(Question)) {
    summaryitems[[i]] <-  summarise_item(data, columns[[i]], Question[i])
  }
  summaryitems <- data.frame(do.call(rbind, summaryitems))
  return(summaryitems)
}

prepare_data_for_plotting <- function(Question, data, answers, columns){
  skeleton <- create_skeleton(Question, answers, columns) # create skeleton of all possible answers
  summaryitems <- bind_summaries_items(Question, data, columns)
  formatted_data <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE) # merge summary items to skeleton
  return(formatted_data)
}

## Prepare barriers data for plotting
create_barriers_skeleton <- function(data, answers, columns){ # doesn't rely on 'Divisions' which now inlcudes GLAM
  
  # example to test function
  # data <- pgrdata_Barriers
  # columns <- c(expr(Barriers_OA), expr(Barriers_Data), expr(Barriers_Code), expr(Barriers_Materials),expr(Barriers_Preprint),expr(Barriers_Prereg),expr(Barriers_RegRep))
  # answers <- c("Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None", "NotSure","NA")
  # create_barriers_skeleton(data, answers, columns)
  
  Div <- rep(sort(unique(data$Div)), each = length(columns)*length(answers)) 
  LabelIndiv <- rep(Measures, each = length(answers), times = length(unique(data$Div))) 
  LabelIndivShort <- rep(Measures_short, each = length(answers), times = length(unique(data$Div)))
  Indiv <-paste(Div, paste( "Barriers", LabelIndivShort, sep = "_"), sep ="_") 
  Answer <- rep(answers, times= length(unique(data$Div))*length(columns)) 
  ID <- paste(Indiv, Answer, sep="_") 
  skeleton <- data.frame(ID, Indiv, Div, LabelIndiv, Answer)
  return(skeleton)
}

summarise_barriers_item <-  function(data, item){
  
  #example to test function
  # data <- pgrdata_Barriers
  # columns <- c(expr(Barriers_OA), expr(Barriers_Data), expr(Barriers_Code), expr(Barriers_Materials),expr(Barriers_Preprint),expr(Barriers_Prereg),expr(Barriers_RegRep))
  # i <- 1
  # item <- columns[[i]]
  # summarise_barriers_item(data, columns[[i]])
  # summarise_barriers_item(pgrdata_Barriers, "Barriers_OA")

  data2 <- subset_columns_by_pattern(data, item) 
  data3 <- data2[rowSums(!is.na(data2)) > 1, ] # remove lines where the item was not scored (all possible answers were left blank, i.e. NA)
  
  NbRespondents <- data3 %>% group_by(Div) %>% summarise(NbRespondents = n()) # number of time the item was scored
  
  data4 <- (data3 %>% 
              group_by(Div) %>% 
              summarise(across (everything(), ~sum(!is.na(.)))) )  %>% # counts of each answer
    
    pivot_longer(!Div, names_to = as.character({{item}}) , values_to = "n")
  
  data5 <- merge(data4, NbRespondents, by = 'Div', all.x = TRUE) 
  data5$perc <- data5$n/data5$NbRespondents * 100 # calculate percentages of respondent having selected (non mutually exclusively) each answer
  data5$ID <- paste(data5$Div, data5$Barriers, sep="_")
  data6 <- data5[,c('ID', 'n', 'perc','NbRespondents')]
  return(data6)
}

bind_summaries_barriers_items <- function(data, columns){
  
  # data <- pgrdata_Barriers
  # columns <- c(expr(Barriers_OA), expr(Barriers_Data), expr(Barriers_Code), expr(Barriers_Materials),expr(Barriers_Preprint),expr(Barriers_Prereg),expr(Barriers_RegRep))
  # i <- 1
  # item <- columns[[i]]
  # summarise_item(data, columns[[i]])
  
  summaryitems <- vector(mode= "list", length = length(columns))
  for (i in 1:length(columns)) {
    summaryitems[[i]] <-  summarise_barriers_item(data, columns[[i]])
  }
  summaryitems <- data.frame(do.call(rbind, summaryitems))
  return(summaryitems)
  
}

prepare_barriers_data_for_plotting <- function(data, answers, columns){
  skeleton <- create_barriers_skeleton(data, answers, columns) # create skeleton of all possible answers
  summaryitems <- bind_summaries_barriers_items(data, columns)
  formatted_data <- merge(skeleton, summaryitems, by = "ID", all.x = TRUE) # merge summary items to skeleton
  return(formatted_data)
}

## Plotting functions
circular_plot_function <- function(data, Question, answers, title_plot, answers_colors) {
 
  # example to test circular_plot_function 
  # data <- pgrdata_Awareness_for_plotting
  # Question <- Measures
  # answers <- Awareness_answers
  # title_plot <- 'Awareness'
  # answers_colors <- Awareness_colors
  
  data <-data[data$Div != "GLAM" & data$Div != "College",] # plot below was designed without GLAM (i.e. missing aestethics e.g. hjust if included)
  
  #name_data_argument <- deparse(substitute(data)) # get the name of the dataset to apply if statement below to move label around depending on plot
  
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Question) # this will determine order of the bars
  
  # Set a number of 'empty bar' to add at the end of each Div
  empty_bar <- 2
  data$Answer <- as.factor(data$Answer)
  data$Div <- as.factor(data$Div)
  nObsType <- nlevels(data$Answer)
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% arrange(Div, LabelIndiv) # this will determine order of the bars
  data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
  rm(to_add)
  
  # Get the name and the y position of each label
  label_data <- data %>% group_by(id, Indiv) %>% summarize(tot=sum(perc, na.rm=TRUE)) # here all at 100% to plot percents and not counts
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  label_data$LabelIndiv <- sapply(strsplit(as.character(label_data$Indiv),"\\_"), `[`, 2)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(Div) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # if (name_data_argument == 'pgrdata_Awareness_for_plotting'){
  # base_data$title[base_data$Div == 'SSD'] <- 15} #for awareness plot
  # 
  # if (name_data_argument == 'pgrdata_Training_for_plotting'){
  # base_data$title[base_data$Div == 'SSD'] <- 51} #for training plot
  
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  
  
  
  
  ## Make the plot
  data_plot <- ggplot(data) +     
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = answers)), stat="identity", alpha=0.5) +
    
    ### Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) + 
    geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 100, xend = number_of_bar, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 75, xend = number_of_bar, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 50, xend = number_of_bar, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 25, xend = number_of_bar, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 0, xend = number_of_bar, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    ### Add text showing the value of each lines max(data$id-0.1)
    ggplot2::annotate("text", x = rep(number_of_bar-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0%", "25%", "50%", "75%", "100%") , color="dimgrey", size=3 , angle=0, fontface="bold", hjust=c(0.5,0.5,0.5,0.5,0.5), vjust = -0.2) +
    
    scale_fill_manual(values = rev(answers_colors), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks = answers, 
                      labels = answers, 
                      drop = FALSE)+
    
    scale_x_discrete(expand = c(0, 0)) +
    ylim(-70,150) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text=element_text(size=13),
      legend.title=element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.border=element_blank(), axis.ticks.length = unit(0, "mm")
    ) +
    
    # guides(fill=guide_legend(nrow=2,byrow=FALSE))+
    
    coord_polar() +
    
    ### Add labels on top of each bar
    geom_text(data=label_data, aes(x=id, y=105, label=LabelIndiv, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4.3, angle= label_data$angle, inherit.aes = FALSE ) + 
    
    ### Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -20, label=Div), hjust=c(1,1,0.5,0, 0), colour = "black", alpha=0.8,  size=4, fontface="bold", inherit.aes = FALSE) + 
    
    ### Add title in the middle
    ggplot2::annotate("text", x = 0, y = -60, label = title_plot , color="black", size=5.5 , angle=0, fontface="bold", hjust=0.5) 
  
  
}

regroup_all_data <- function(splitdata){
  All_data <- splitdata[,c("LabelIndiv", "Answer", "n")] %>% group_by(LabelIndiv, Answer) %>% summarise (n = sum(n, na.rm=TRUE)) 
  All_data <- All_data %>% group_by(LabelIndiv) %>% mutate(perc = n / sum(n) * 100 )
  return(All_data)
}

stacked_barplot_on_regrouped_data <- function(All_data, Question, answers, answers_colors){
  All_data$LabelIndiv <- factor(All_data$LabelIndiv, levels = Question) # this will determine order of the bars
  ggplot(All_data) +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                   level = answers)),
             stat="identity", alpha=0.5) +
    
    scale_fill_manual(values = rev(answers_colors), 
                      breaks=answers, 
                      labels =answers, 
                      drop = FALSE)+
    
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(angle = 90),
      legend.title=element_blank())
  
}  

horizontal_stack_barplot_per_ORP <- function(data, divisions, Question, answers, answers_colors, title_legend, title_plot){
  
  # example to test function
  # data <- pgrdata_Awareness_for_plotting
  # answers <- Awareness_answers
  # answers_colors <- Awareness_colors
  # title_legend <- "PGR students"
  # title_plot <- NULL
  
  data <- data[data$Div %in% Plotted_Div,]
  
  data$Div <- factor(data$Div, levels = rev(Divisions)) # this will determine order of the bars
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Question) # this will determine order of the bars
  
  count_by_answer_and_div_and_orp <- data %>% 
    group_by(Answer, Div, LabelIndiv) %>%
    #summarise(num_respondents = sum(n, na.rm = TRUE)) %>% 
    mutate(Answer = factor(Answer, levels = answers))
  
  count_by_answer_and_div_and_orp %>% 
    ggplot() +
    geom_bar(aes(x = Div, y = perc, fill = Answer), stat = "identity", position = "fill") +
    scale_fill_manual(values = (answers_colors),
                      breaks = rev(answers), 
                      labels = rev(answers), 
                      drop = FALSE) +
    facet_wrap(~LabelIndiv, scales = "free_x", ncol = 1) +
    coord_flip() +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position="right",
          #legend.title = element_blank(),
          strip.text.x = element_text(size = 10, colour = "black")) + 
    labs(x = "", y = "")+
    guides(fill=guide_legend(title=title_legend))+
    ggtitle(title_plot) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
  
}

## Plotting functions for recruitment criteria
horizontal_stacked_barplot_on_regrouped_data <- function(All_data, Question, answers, answers_colors, title_plot, legend_position){
  All_data$LabelIndiv <- factor(All_data$LabelIndiv, levels = rev(Question)) # this will determine order of the bars
  
  All_data %>% 
    ggplot() +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=(perc/100), fill=factor(Answer, level = answers)),
             stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = rev(answers_colors), 
                      breaks=answers, 
                      labels =answers, 
                      drop = FALSE) +
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = legend_position, 
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title=element_blank(),
      axis.text.y = element_text(size = 11, colour = "black"),
      plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) + 
    ggtitle(title_plot) + 
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))
}

horizontal_stacked_barplot_on_regrouped_data_x_right <- function(All_data, Question, answers, answers_colors, title_plot){
  All_data$LabelIndiv <- factor(All_data$LabelIndiv, levels = rev(Question)) # this will determine order of the bars
  
  All_data %>% 
    ggplot() +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=(perc/100), fill=factor(Answer, level = answers)),
             stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = rev(answers_colors), 
                      breaks=answers, 
                      labels =answers, 
                      drop = FALSE) +
    scale_x_discrete(position = "top")+
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = "none", 
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title=element_blank(),
      axis.text.y = element_text(size = 11, colour = "black"),
      plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) + 
    ggtitle(title_plot) + 
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))
}

horizontal_stacked_barplot_on_regrouped_data_without_axis_text <- function(All_data, Question, answers, answers_colors, title_plot){
  All_data$LabelIndiv <- factor(All_data$LabelIndiv, levels = rev(Question)) # this will determine order of the bars
  
  All_data %>% 
    ggplot() +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=(perc/100), fill=factor(Answer, level = answers)),
             stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = rev(answers_colors), 
                      breaks=answers, 
                      labels =answers, 
                      drop = FALSE) +
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = "none", 
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title=element_blank(),
      axis.text.y = element_blank(),
      plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) + 
    ggtitle(title_plot) + 
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))
}

extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# Plotting functions for barriers
barriers_circular_plot_function <- function(data){
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Measures) # this will determine order of the bars
  
  
  # Set a number of 'empty bar' to add at the end of each Div
  empty_bar <- 2
  data$Answer <- as.factor(data$Answer)
  data$Div <- as.factor(data$Div)
  nObsType <- nlevels(data$Answer)
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Div)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$Div <- rep(levels(data$Div), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% arrange(Div, LabelIndiv)  # this will determine order of the bars
  data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
  rm(to_add)
  
  # Get the name and the y position of each label
  label_data <- data %>% group_by(id, Indiv) %>% summarize(tot=sum(perc, na.rm=TRUE)) # here all at 100% to plot percents and not counts
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  label_data <- merge(label_data, unique(data[,c('Indiv', 'LabelIndiv')]), all.x= TRUE, by = 'Indiv')
  
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(Div) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  base_data2 <- base_data
  base_data2$title[base_data$Div == 'SSD'] <- 39
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  
  
  
  
  ## Make the plot
  pgrdata_Barriers_plot <- ggplot(data) +      
    
    ### Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=perc, fill=factor(Answer, level = c("None", "Other", "Policy", "Incentives", "Norms" , "Training", "Infrastructure", "NotSure", "NA"))), stat="identity", alpha=0.5) +
    
    ### Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) + 
    geom_segment(data=grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 100, xend = number_of_bar, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 75, xend = number_of_bar, yend = 75), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 50, xend = number_of_bar, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 25, xend = number_of_bar, yend = 25), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = number_of_bar-1, y = 0, xend = number_of_bar, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    ### Add text showing the value of each lines max(data$id-0.1)
    ggplot2::annotate("text", x = rep(number_of_bar-0.5,5), y = c(0, 25, 50, 75, 100), label = c("0%", "25%", "50%", "75%", "100%") , color="grey", size=3 , angle=0, fontface="bold", hjust=c(0.5,0.5,0.5,0.5,0.5), vjust = -0.2) +
    
    
    scale_fill_manual(values = c("black", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("NA", "NotSure", "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None"),
                      labels =c("Not applicable", "Not sure",  "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None")
                      , drop = FALSE)+
    
    scale_x_discrete(expand = c(0, 0)) + # remove padding
    ylim(-100,max(label_data$tot, na.rm=T)+30) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title=element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
      # plot.margin = unit(rep(1,4), "cm") 
    ) +
    coord_polar() +
    
    ### Add labels on top of each bar
    geom_text(data=label_data, aes(x=id, y=tot+10, label=LabelIndiv, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
    
    ### Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data2, aes(x = title, y = -20, label=Div), hjust=c(1,1,0.5,0, 0), vjust=c(0.5,0.5,0.5,0.5, 1), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
    
    ### Add title in the middle
    ggplot2::annotate("text", x = 0, y = -90, label = "Barriers" , color="black", size=5 , angle=0, fontface="bold", hjust=0.5) 
  
}

regroup_all_barriers_data <- function(splitdata){
  
  # example to test function
  # splitdata <- pgrdata_Barriers_for_plotting

  All_data <- splitdata[,c("LabelIndiv", "Answer", "n", "NbRespondents")] %>% group_by(LabelIndiv, Answer) %>% summarise (n = sum(n, na.rm=TRUE),NbRespondents = sum(NbRespondents, na.rm=TRUE)) 
  All_data <- All_data  %>% group_by(LabelIndiv) %>% mutate(perc = n / NbRespondents * 100 )
  return(All_data)
  
}

stacked_barplot_on_barriers_regrouped_data <- function(All_data, Question, answers){
  All_data$LabelIndiv <- factor(All_data$LabelIndiv, levels = Question) # this will determine order of the bars
  ggplot(All_data) +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                   level = answers)),
             stat="identity", alpha=0.5) +
    
    scale_fill_manual(values = c("black", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("NA", "NotSure", "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None"),
                      labels =c("Not applicable", "Not sure",  "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None")
                      , drop = FALSE)+
    
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(angle = 90),
      legend.title=element_blank()) + guides(fill = guide_legend(reverse = TRUE))
  
}  

dodged_barplot_on_barriers_regrouped_data <- function(All_data, Question, answers){
  All_data$LabelIndiv <- factor(All_data$LabelIndiv, levels = Question) # this will determine order of the bars
  ggplot(All_data) +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                   level = answers)),
             stat="identity", alpha=0.5, position = "dodge") +
    
    scale_fill_manual(values = c("black", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("NA", "NotSure", "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None"),
                      labels =c("Not applicable", "Not sure",  "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None")
                      , drop = FALSE)+
    
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(angle = 90),
      legend.title=element_blank()) + guides(fill = guide_legend(reverse = TRUE))
  
}  

horizontal_dodged_barplot_on_barriers_regrouped_data <- function(All_data, Question, answers, title_plot, plot_ylim){
  All_data$LabelIndiv <- factor(All_data$LabelIndiv, levels = rev(Question)) # this will determine order of the bars
  ggplot(All_data) +
    
    ### Add the stacked bar
    geom_bar(aes(x=LabelIndiv, y=perc, fill=factor(Answer, 
                                                   level = answers)),
             stat="identity", position = "dodge") +
       scale_fill_manual(values = c("black", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("NA", "NotSure", "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None"),
                      labels =c("Not applicable", "Not sure",  "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None")
                      , drop = FALSE)+
    
    coord_flip(ylim = c(0, plot_ylim)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80), label = c("0%", "20%", "40%", "60%", "80%"))+
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5),
     # axis.text.x = element_text(angle = 90),
            legend.title=element_blank()) + guides(fill = guide_legend(reverse = FALSE))+
    ggtitle(title_plot)
  
}  

barriers_horizontal_stack_barplot_per_ORP <- function(data, Question, answers, answers_colors, title_legend, title_plot, plot_ylim){
  
  # example to test function
  # data <- pgrdata_Barriers_for_plotting
  # answers <- Barriers_answers
  # answers_colors <- Barriers_colors
  # title_legend <- "PGR students"
  # title_plot <- NULL
  
  data$Div <- factor(data$Div, levels = rev(Divisions)) # this will determine order of the bars
  data$Answer <- factor(data$Answer, levels = answers) # this will determine order of the answers
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Question) # this will determine order of the bars
  
  data %>% 
    ggplot() +
    geom_bar(aes(x = Div, y = perc, fill = Answer), stat = "identity") +
    scale_fill_manual(values = c("black", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("NA", "NotSure", "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None"),
                      labels =c("Not applicable", "Not sure",  "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None")
                      , drop = FALSE)+
    facet_wrap(~LabelIndiv, scales = "free_x", ncol = 1) +
    coord_flip(ylim = c(0, plot_ylim)) +
    theme_minimal() +
        theme(legend.position="bottom",
          #legend.title = element_blank(),
          strip.text.x = element_text(size = 10, colour = "black")) + 
    labs(x = "", y = "")+
    guides(fill=guide_legend(title=title_legend))+
    ggtitle(title_plot) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
  
}

barriers_horizontal_dodge_barplot_per_ORP <- function(data, divisions, Question, answers, answers_colors, title_legend, title_plot, plot_ylim){

  data <- data[data$Div %in% Plotted_Div,]
  
  data$Div <- factor(data$Div, levels = rev(Divisions)) # this will determine order of the bars
  data$Answer <- factor(data$Answer, levels = answers) # this will determine order of the answers
  data$LabelIndiv <- factor(data$LabelIndiv, levels = Question) # this will determine order of the bars
  
  data %>% 
    ggplot() +
    geom_bar(aes(x = Div, y = perc, fill = Answer), stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("black", "#666666", "#E31A1C", "#FC4E2A", "#FD8D3C", "#FEB24C", "#FED976", "#FFEDA0", "#B8E186"), # https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
                      breaks=c("NA", "NotSure", "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None"),
                      labels =c("Not applicable", "Not sure",  "Infrastructure", "Training", "Norms" , "Incentives", "Policy", "Other", "None")
                      , drop = FALSE)+
    facet_wrap(~LabelIndiv, scales = "free_x", ncol = 1) +
    coord_flip(ylim = c(0, plot_ylim)) +
    theme_minimal() +
    theme(legend.position="bottom",
          #legend.title = element_blank(),
          strip.text.x = element_text(size = 10, colour = "black")) + 
    labs(x = "", y = "")+
    guides(fill=guide_legend(title=title_legend))+
    ggtitle(title_plot) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
  
}


## analyse text
capitalise_all_strings <- function(data){
  data.frame(lapply(data, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}), stringsAsFactors=FALSE)
}

prepare_freetext_subdataset <- function(data, pattern){
  # example to test function
  # data <- pgrdata
  # pattern <- "^OtherBarriers_"
  subdataset <- subset_columns_by_pattern(data, pattern)
  subdataset <- subdataset[rowSums(!is.na(subdataset)) > 1, ] # keep rows with at least one entry in the row
  subdataset <- capitalise_all_strings(subdataset)
  colnames(subdataset) <- str_remove(colnames(subdataset), pattern)
  return(subdataset)
}

create_list_for_checking_cat <- function (data){
  
  #example to test function
  #data <- pgrdata_OB
  #data <- pgrdata_WD
  
  data <- add_column(data, ID = 1:nrow(data), .before = 1)
  
  colnameswithcat_all <- colnames(data[,grep(pattern=".*cat", x=colnames(data))])
  colnameswithcat_1 <- colnames(data[,grep(pattern=".*cat$", x=colnames(data))])
  colnameswithcat_23 <- colnames(data[,grep(pattern=".*cat.$", x=colnames(data))])
  
  # merge pivot table of values with their first cat 
  a_values_1 <- pivot_longer(data[,!colnames(data) %in% colnameswithcat_all], -c(ID, Div), values_to = "Value", names_to = "Measure")
  a_cat_1 <- pivot_longer(data[,colnames(data) %in% colnameswithcat_1],  colnameswithcat_1, values_to = "Category", names_to = "Measure")
  a_1 <- cbind(a_values_1, a_cat_1[,c('Category')])
  
  # add all the extra columns for values that were cat 2 or 3 
  a_values_data2 <- pivot_longer(data[!is.na(data$Data_cat2),c('ID', 'Div','Data')], -c(ID, Div), values_to = "Value", names_to = "Measure")
  a_values_prereg2 <- pivot_longer(data[!is.na(data$Prereg_cat2),c('ID', 'Div','Prereg')], -c(ID, Div), values_to = "Value", names_to = "Measure")
  a_values_regrep2 <- pivot_longer(data[!is.na(data$RegRep_cat2),c('ID', 'Div','RegRep')], -c(ID, Div), values_to = "Value", names_to = "Measure")
  a_values_regrep3 <- pivot_longer(data[!is.na(data$RegRep_cat3),c('ID', 'Div','RegRep')], -c(ID, Div), values_to = "Value", names_to = "Measure")
  a_values_preprint2 <- pivot_longer(data[!is.na(data$Preprint_cat2),c('ID', 'Div','Preprint')], -c(ID, Div), values_to = "Value", names_to = "Measure")
  
  a_data2 <- cbind(a_values_data2, Category = data$Data_cat2[!is.na(data$Data_cat2)])
  a_prereg2 <- cbind(a_values_prereg2, Category = data$Prereg_cat2[!is.na(data$Prereg_cat2)])
  a_regrep2 <- cbind(a_values_regrep2, Category = data$RegRep_cat2[!is.na(data$RegRep_cat2)])
  a_regrep3 <- cbind(a_values_regrep3, Category = data$RegRep_cat3[!is.na(data$RegRep_cat3)])
  a_preprint2 <- cbind(a_values_preprint2, Category = data$Preprint_cat2[!is.na(data$Preprint_cat2)])
  
  a <- rbind(a_1, a_data2, a_prereg2, a_regrep2, a_regrep3,a_preprint2)
  a <- a[!is.na(a["Value"]),]
  a <- a[with(a,order(a$ID,a$Measure)),]
  rm(a_values_1,a_cat_1, a_1, a_values_data2, a_values_prereg2, a_values_regrep2, a_values_regrep3, a_values_preprint2,
     a_data2, a_prereg2,a_regrep2, a_regrep3, a_preprint2,
     colnameswithcat_all, colnameswithcat_1, colnameswithcat_23)
  return(a)
  } # to update if more cat2 and cat3
  
create_pivot_table_from_list_for_checking_cat <- function (a){
  
  #example to test function
  #a <- a_allstaffdata_OB

  name_data_argument <- deparse(substitute(a)) # get the name of the dataset to apply if statement below to sort columns in the table
  
  a$Measure[a$Measure == 'OA'] <- "Open Access" 
  a$Measure[a$Measure == 'Data'] <- "Open Data" 
  a$Measure[a$Measure == 'Code'] <- "Open Code" 
  a$Measure[a$Measure == 'Materials'] <- "Open Materials" 
  a$Measure[a$Measure == 'Preprint'] <- "Preprint" 
  a$Measure[a$Measure == 'Prereg'] <- "Preregistration"
  a$Measure[a$Measure == 'RegRep'] <- "Registered Report"
  a$Measure<- factor(a$Measure, levels = Measures)
  
  b <- a %>% group_by(Measure, Category) %>% summarise(count = n()) 
  c <- dcast(b, Category ~ Measure, value.var = "count") # from reshape2
  c$Total <- rowSums(c[,-1], na.rm=TRUE)
  
  if (name_data_argument != 'a_allstaffdata_OB'){
  c <- c[with(c, order(-c$Total,
                       -c$`Open Access`,
                       -c$`Open Data`,
                       -c$`Open Code`,
                       -c$`Open Materials`,
                       -c$Preprint,
                       -c$Preregistration,
                       -c$`Registered Report`)),]
  }
  else {
    c <- c[with(c, order(-c$Total,
                         -c$`Open Access`,
                         -c$`Open Data`,
                         -c$`Open Code`,
                         -c$Preregistration)),]
  }
  
  c[is.na(c)] <- '-'
  d <- c[c$Category != 'Not categorised',]
  colnames(d)[colnames(d) == 'Category'] <- ''
  rownames(d) <- NULL
  pivot_table <- d
  rm(a,b,c,d)
  
  return(pivot_table)
}


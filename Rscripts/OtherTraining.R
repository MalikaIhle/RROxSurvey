
  #source("Rscripts/FormatData.R")

Training_answers <- c("Written guidance and workshop-led training", "Written guidance only", "No guidance wanted", "No guidance needed",  "Not sure",  "Not applicable" )

# pgrdata_OtherTraining 
## subset and reformat data
pgrdata_OtherTraining <- subset_columns_by_pattern(pgrdata, "^Training_Other")
pgrdata_OtherTraining <- pgrdata_OtherTraining[rowSums(!is.na(pgrdata_OtherTraining)) > 1, ]
colnames(pgrdata_OtherTraining) <- c('Div', 'Training_Other_score', 'Training_Other', 'Training_Other_score', 'Training_Other','Training_Other_score', 'Training_Other')
pgrdata_OtherTraining <- rbind(pgrdata_OtherTraining[,c(1,2,3)], pgrdata_OtherTraining[,c(1,4,5)], pgrdata_OtherTraining[, c(1,6,7)])
pgrdata_OtherTraining <- pgrdata_OtherTraining[!is.na(pgrdata_OtherTraining$Training_Other),]
### Nb of respondents
nrow(pgrdata_OtherTraining)

## recode
pgrdata_OtherTraining$Training_Other_recode[str_detect(pgrdata_OtherTraining$Training_Other, "superv")] <- 'how to supervise/mentor/teach/collaborate'
pgrdata_OtherTraining$Training_Other_recode[str_detect(pgrdata_OtherTraining$Training_Other, "employable skill")] <- 'How to ensure that you have acquired employable skill'
pgrdata_OtherTraining$Training_Other_recode[pgrdata_OtherTraining$Training_Other == 'How to negotiate with publishers about book publications'] <- 'How to negotiate with publishers about book publications'
pgrdata_OtherTraining$Training_Other_recode[str_detect(pgrdata_OtherTraining$Training_Other, 'diversity|Diverse|Social Justice')] <- 'How to foster diversity and social justice'

pgrdata_OtherTraining[,c('Div','Training_Other_score','Training_Other_recode')]

pgrdata_OtherTraining$Training_Other_score <- factor(pgrdata_OtherTraining$Training_Other_score , levels = Training_answers)

xtab_OtherTraining <- pgrdata_OtherTraining %>% 
  tabyl(Training_Other_recode, Training_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-`Written guidance and workshop-led training`)
names(xtab_OtherTraining)[1] <- "" 




# staffdata_OtherTraining 
## subset and reformat data
staffdata_OtherTraining <- subset_columns_by_pattern(staffdata, "^Training_Other")
staffdata_OtherTraining <- staffdata_OtherTraining[rowSums(!is.na(staffdata_OtherTraining)) > 1, ]
colnames(staffdata_OtherTraining) <- c('Div', 'Training_Other_score', 'Training_Other', 'Training_Other_score', 'Training_Other','Training_Other_score', 'Training_Other')
staffdata_OtherTraining <- rbind(staffdata_OtherTraining[,c(1,2,3)], staffdata_OtherTraining[,c(1,4,5)], staffdata_OtherTraining[, c(1,6,7)])
staffdata_OtherTraining <- staffdata_OtherTraining[!is.na(staffdata_OtherTraining$Training_Other),]
### Nb of respondents
nrow(staffdata_OtherTraining)
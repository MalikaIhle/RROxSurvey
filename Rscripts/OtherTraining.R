
  #source("Rscripts/FormatData.R")

Training_answers <- c("Written guidance and workshop-led training", "Written guidance only", "No guidance wanted", "No guidance needed",  "Not sure",  "Not applicable" )

# Alldata_OtherTraining 
## subset, merge, and reformat data
pgrdata_OtherTraining <- cbind(subset_columns_by_pattern(pgrdata, "^Training_Other"), Subdataset = 'pgrdata')
staffdata_OtherTraining <- cbind(subset_columns_by_pattern(staffdata, "^Training_Other"), Subdataset = 'staffdata')
supportstaffdata_OtherTraining <- cbind(subset_columns_by_pattern(supportstaffdata, "^Training_Other"), Subdataset = 'supportstaffdata')
academicdata_OtherTraining <- cbind(subset_columns_by_pattern(academicdata, "^Training_Other"), Subdataset = 'academicdata')

Alldata_OtherTraining <- rbind(pgrdata_OtherTraining, staffdata_OtherTraining, supportstaffdata_OtherTraining, academicdata_OtherTraining)
Alldata_OtherTraining <- Alldata_OtherTraining[rowSums(!is.na(Alldata_OtherTraining)) > 2, ]
colnames(Alldata_OtherTraining) <- c('Div', 'Training_Other_score', 'Training_Other', 'Training_Other_score', 'Training_Other','Training_Other_score', 'Training_Other', 'Subdataset')
Alldata_OtherTraining <- rbind(Alldata_OtherTraining[,c(1,2,3,8)], Alldata_OtherTraining[,c(1,4,5,8)], Alldata_OtherTraining[, c(1,6,7,8)])
Alldata_OtherTraining <- Alldata_OtherTraining[!is.na(Alldata_OtherTraining$Training_Other),]
### Nb of respondents
nrow(Alldata_OtherTraining)

## recode
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, "superv")] <- 'How to supervise/mentor/teach/collaborate'
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, "employable skill")] <- 'How to ensure that you have acquired employable skill'
Alldata_OtherTraining$Training_Other_recode[Alldata_OtherTraining$Training_Other == 'How to negotiate with publishers about book publications'] <- 'How to negotiate with publishers about book publications'
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, 'diversity|Diverse|Social Justice')] <- 'How to foster diversity and social justice'


Alldata_OtherTraining$Training_Other_score <- factor(Alldata_OtherTraining$Training_Other_score , levels = Training_answers)

## split per subdataset
xtab_OtherTraining <- Alldata_OtherTraining %>% 
  tabyl(Training_Other_recode, Training_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-`Written guidance and workshop-led training`)
names(xtab_OtherTraining)[1] <- "" 
xtab_OtherTraining

pgrdata_xtab_OtherTraining <- Alldata_OtherTraining[Alldata_OtherTraining$Subdataset == 'pgrdata',] %>% 
  tabyl(Training_Other_recode, Training_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-`Written guidance and workshop-led training`)
names(pgrdata_xtab_OtherTraining)[1] <- "" 
pgrdata_xtab_OtherTraining


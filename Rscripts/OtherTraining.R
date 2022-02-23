
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
Alldata_OtherTraining$Training_Other <- toupper(Alldata_OtherTraining$Training_Other)
### Nb of respondents
nrow(Alldata_OtherTraining)

## recode
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, "SUPERV")] <- 'How to supervise/mentor/teach/collaborate'
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, "EMPLOYABLE SKILL")] <- 'How to ensure that you have acquired employable skill'
Alldata_OtherTraining$Training_Other_recode[Alldata_OtherTraining$Training_Other == 'HOW TO NEGOTIATE WITH PUBLISHERS ABOUT BOOK PUBLICATIONS'] <- 'How to negotiate with publishers about book publications'
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, 'DIVERSITY|DIVERSE|SOCIAL JUSTICE')] <- 'How to foster diversity and social justice'
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, 'APPROPRIATENESS|CAN AND CAN\'T BE SHARED')] <- 'Guidance on what can and cannot be shared'
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, 'JOB APPLICATIONS')] <- 'How to prepare job application to highligth contribution to open-science, mentorship, etc'
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, 'ASSESS PUBLICATIONS')] <- 'How to assess publication without making IF the primary indicator'
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, 'TISSUE BANKS')] <- 'How to prepare sample or access samples from tissue banks'
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, 'PAY FOR OPEN ACCESS')] <- 'How to pay for open access'
Alldata_OtherTraining$Training_Other_recode[str_detect(Alldata_OtherTraining$Training_Other, 'DIGITAL PRESERVATION')] <- 'Guidance on data migration and digital preservation'


Alldata_OtherTraining$Training_Other_recode[!is.na(Alldata_OtherTraining$Training_Other) & is.na(Alldata_OtherTraining$Training_Other_recode)] <- 'Not categorised'
Alldata_OtherTraining[!is.na(Alldata_OtherTraining$Training_Other) & Alldata_OtherTraining$Training_Other_recode == 'Not categorised',]
Alldata_OtherTraining$Training_Other_score <- factor(Alldata_OtherTraining$Training_Other_score , levels = Training_answers)

## split per subdataset
xtab_OtherTraining <- Alldata_OtherTraining[Alldata_OtherTraining$Training_Other_recode != 'Not categorised',] %>% 
  tabyl(Training_Other_recode, Training_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-`Written guidance and workshop-led training`)
names(xtab_OtherTraining)[1] <- "Training topics" 
xtab_OtherTraining

pgrdata_xtab_OtherTraining <- Alldata_OtherTraining[Alldata_OtherTraining$Subdataset == 'pgrdata' & Alldata_OtherTraining$Training_Other_recode != 'Not categorised',] %>% 
  tabyl(Training_Other_recode, Training_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-`Written guidance and workshop-led training`)
names(pgrdata_xtab_OtherTraining)[1] <- "" 
pgrdata_xtab_OtherTraining


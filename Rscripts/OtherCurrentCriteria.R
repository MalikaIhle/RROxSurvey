
  #source("Rscripts/FormatData.R")

Criteria_answers <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")

# Alldata_OtherCurrentRecruitment 
## subset, merge, and reformat data
pgrdata_OtherCurrentRecruitment <- cbind(subset_columns_by_pattern(pgrdata, "^CurrentRecruitment_Other"), Subdataset = 'pgrdata')
staffdata_OtherCurrentRecruitment <- cbind(subset_columns_by_pattern(staffdata, "^CurrentRecruitment_Other"), Subdataset = 'staffdata')
supportstaffdata_OtherCurrentRecruitment <- cbind(subset_columns_by_pattern(supportstaffdata, "^CurrentRecruitment_Other"), Subdataset = 'supportstaffdata')
academicdata_OtherCurrentRecruitment <- cbind(subset_columns_by_pattern(academicdata, "^CurrentRecruitment_Other"), Subdataset = 'academicdata')

Alldata_OtherCurrentRecruitment <- rbind(pgrdata_OtherCurrentRecruitment, staffdata_OtherCurrentRecruitment, supportstaffdata_OtherCurrentRecruitment, academicdata_OtherCurrentRecruitment)
Alldata_OtherCurrentRecruitment <- Alldata_OtherCurrentRecruitment[rowSums(!is.na(Alldata_OtherCurrentRecruitment)) > 2, ]
colnames(Alldata_OtherCurrentRecruitment) <- c('Div', 'CurrentRecruitment_Other_score', 'CurrentRecruitment_Other', 'CurrentRecruitment_Other_score', 'CurrentRecruitment_Other','CurrentRecruitment_Other_score', 'CurrentRecruitment_Other', 'Subdataset')
Alldata_OtherCurrentRecruitment <- rbind(Alldata_OtherCurrentRecruitment[,c(1,2,3,8)], Alldata_OtherCurrentRecruitment[,c(1,4,5,8)], Alldata_OtherCurrentRecruitment[, c(1,6,7,8)])
Alldata_OtherCurrentRecruitment <- Alldata_OtherCurrentRecruitment[!is.na(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other),]
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other <- toupper(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other)
### Nb of respondents
nrow(Alldata_OtherCurrentRecruitment)


## recode
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'REFERENCE|CONNECTION|WHO YOU KNOW|KNOWING PEOPLE|NETWORK*|ALUMNI|AWARDING INSTITUTION|REFEREES|REPUTATION OF FORMER SUPERVISOR')] <- 'Personal connections (e.g. academic references, people from the hiring department, reputaiton of former supervisor)'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'PUBLIC ENGAGEMENT|SOCIAL MEDIA')] <- 'Public engagement and social media presence'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'DIVERSITY')] <- 'Diversity'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'LEADING MAJOR FIELDWORK PROJECTS')] <- 'Leading major fieldwork projects'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'UNDERSERVED|NOVELTY|INTERESTS IN COMMON|COMPLEMENTARITY')] <- 'Having novel, underserved or complementary expertise in the department'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'CONFERENCE')] <- 'Conference speaking'

Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[!is.na(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other) & is.na(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode)] <- 'Not categorised'

Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_score <- factor(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_score , levels = Criteria_answers)


## split per subdataset

xtab_OtherCurrentRecruitment <- Alldata_OtherCurrentRecruitment %>% 
  tabyl(CurrentRecruitment_Other_recode, CurrentRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(xtab_OtherCurrentRecruitment)[1] <- "" 
xtab_OtherCurrentRecruitment

pgrdata_xtab_OtherCurrentRecruitment <- Alldata_OtherCurrentRecruitment[Alldata_OtherCurrentRecruitment$Subdataset == 'pgrdata',] %>% 
  tabyl(CurrentRecruitment_Other_recode, CurrentRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(xtab_OtherCurrentRecruitment)[1] <- "" 
pgrdata_xtab_OtherCurrentRecruitment


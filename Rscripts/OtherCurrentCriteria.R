
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
### Nb of respondents
nrow(Alldata_OtherCurrentRecruitment)


## recode
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'references|connection|Who you know|Connections|Knowing people')] <- 'Personal connections (e.g. academic references, people from the hiring department'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Public engagement|Social media')] <- 'Public engagement and social media presence'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Diversity')] <- 'Diversity'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Leading major fieldwork projects')] <- 'Leading major fieldwork projects'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'underserved')] <- 'Having expertise underserved in the department'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Academic Background')] <- 'Academic Background (not sure what is meant here)'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Conference')] <- 'Conference speaking'

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


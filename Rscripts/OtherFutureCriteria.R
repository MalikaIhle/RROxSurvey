
  #source("Rscripts/FormatData.R")

Criteria_answers <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")

# Alldata_OtherFutureRecruitment 
## subset, merge, and reformat data
pgrdata_OtherFutureRecruitment <- cbind(subset_columns_by_pattern(pgrdata, "^FutureRecruitment_Other"), Subdataset = 'pgrdata')
staffdata_OtherFutureRecruitment <- cbind(subset_columns_by_pattern(staffdata, "^FutureRecruitment_Other"), Subdataset = 'staffdata')
supportstaffdata_OtherFutureRecruitment <- cbind(subset_columns_by_pattern(supportstaffdata, "^FutureRecruitment_Other"), Subdataset = 'supportstaffdata')
academicdata_OtherFutureRecruitment <- cbind(subset_columns_by_pattern(academicdata, "^FutureRecruitment_Other"), Subdataset = 'academicdata')

Alldata_OtherFutureRecruitment <- rbind(pgrdata_OtherFutureRecruitment, staffdata_OtherFutureRecruitment, supportstaffdata_OtherFutureRecruitment, academicdata_OtherFutureRecruitment)
Alldata_OtherFutureRecruitment <- Alldata_OtherFutureRecruitment[rowSums(!is.na(Alldata_OtherFutureRecruitment)) > 2, ]
colnames(Alldata_OtherFutureRecruitment) <- c('Div', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other','FutureRecruitment_Other_score', 'FutureRecruitment_Other', 'Subdataset')
Alldata_OtherFutureRecruitment <- rbind(Alldata_OtherFutureRecruitment[,c(1,2,3,8)], Alldata_OtherFutureRecruitment[,c(1,4,5,8)], Alldata_OtherFutureRecruitment[, c(1,6,7,8)])
Alldata_OtherFutureRecruitment <- Alldata_OtherFutureRecruitment[!is.na(Alldata_OtherFutureRecruitment$FutureRecruitment_Other),]
### Nb of respondents
nrow(Alldata_OtherFutureRecruitment)

## recode
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'references|connection|Who you know|Connections|Knowing people')] <- 'Personal connections (e.g. people from the hiring department)'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'Public engagement|Social media')] <- 'Public engagement'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'Diversity|Social Justice')] <- 'Diversity'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'fieldwork projects')] <- 'Leading major fieldwork projects'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'Innovation|Originality')] <- 'Originality of research'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'Communication skills')] <- 'Communication skills (explaining their research)'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'medical impact')] <- 'Medical impact'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'Availability of teaching in previous career')] <- 'Availability of teaching in previous career'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'Conference')] <- 'Conference speaking'

Alldata_OtherFutureRecruitment$FutureRecruitment_Other_score <- factor(Alldata_OtherFutureRecruitment$FutureRecruitment_Other_score , levels = Criteria_answers)


## split per subdataset

xtab_OtherFutureRecruitment <- Alldata_OtherFutureRecruitment %>% 
  tabyl(FutureRecruitment_Other_recode, FutureRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(xtab_OtherFutureRecruitment)[1] <- "" 
xtab_OtherFutureRecruitment


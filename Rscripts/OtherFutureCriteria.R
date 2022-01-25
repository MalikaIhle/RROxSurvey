
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
Alldata_OtherFutureRecruitment$FutureRecruitment_Other <- toupper(Alldata_OtherFutureRecruitment$FutureRecruitment_Other)
### Nb of respondents
nrow(Alldata_OtherFutureRecruitment)

## recode
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'REFERENCE|CONNECTION|WHO YOU KNOW|KNOWING PEOPLE|NETWORK*|ALUMNI|AWARDING INSTITUTION|REFEREES|REPUTATION OF FORMER SUPERVISOR')] <- 'Personal connections (e.g. people from the hiring department)'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'PUBLIC ENGAGEMENT|SOCIAL MEDIA')] <- 'Public engagement'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'DIVERSITY|SOCIAL JUSTICE')] <- 'Diversity'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'FIELDWORK PROJECTS')] <- 'Leading major fieldwork projects'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'INNOVATION|ORIGINALITY|IDEAS|VISION|CREATIVE|PROPOSAL STRENGTH|AMBITION')] <- 'Originality, strenght, or ambition of research'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'COMMUNICATION SKILLS')] <- 'Communication skills (explaining their research)'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'MEDICAL IMPACT')] <- 'Medical impact'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'AVAILABILITY OF TEACHING IN PREVIOUS CAREER')] <- 'Availability of teaching in previous career'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'CONFERENCE')] <- 'Conference speaking'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'REPLICATION')] <- 'Number of successful replications'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'INTERDISCIPLINARITY')] <- 'Interdisciplinarity'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'BUTTONS|PARKING|DIFFICULT TO EVALUATE')] <- 'Not categorised'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'TEAM WORK RECORD')] <- 'Collaborative skills'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'ABILITY TO TEACH IN THE LANGUAGE')] <- 'Specfic skills or knowledge'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'PERFORMANCE IN THE INTERVIEW')] <- 'Performance during the interview'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'MOTIVATION')] <- 'Motivation'

Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[!is.na(Alldata_OtherFutureRecruitment$FutureRecruitment_Other) & is.na(Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode)] <- 'Not categorised'

Alldata_OtherFutureRecruitment$FutureRecruitment_Other_score <- factor(Alldata_OtherFutureRecruitment$FutureRecruitment_Other_score , levels = Criteria_answers)


## split per subdataset

xtab_OtherFutureRecruitment <- Alldata_OtherFutureRecruitment %>% 
  tabyl(FutureRecruitment_Other_recode, FutureRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(xtab_OtherFutureRecruitment)[1] <- "" 
xtab_OtherFutureRecruitment


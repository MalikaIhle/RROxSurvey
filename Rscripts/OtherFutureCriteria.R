
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
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'BEING MATES|REFERENCE|CONNECTION|WHO YOU KNOW|KNOWING PEOPLE|NETWORK*|ALUMNI|AWARDING INSTITUTION|PRESTIGE OF PAST INSTITUTION|REFEREES|REPUTATION OF FORMER SUPERVISOR|WITH A SENIOR MEMBER OF THE DEPARTMENT')] <- 'Personal connections to hiring department and prestige of former connections'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'SOCIAL MEDIA|ALTMETRICS')] <- 'Social media presence'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'PUBLIC ENGAGEMENT|OUTREACH|PASTORAL ABILITIES')] <- 'Public engagement and outreach' # need this one to be below social media to overwrite one entry
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'INNOVATION|UNDERSERVED|NOVELTY|INTERESTS IN COMMON|COMPLEMENTARITY|ORIGINALITY|RESEARCH MOTIVATION AND INTEREST|IDEAS|CREATIVE THINKING|QUESTION THE STATUS QUO')] <- 'Having novel, underserved, or complementary expertise from the department'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'PAST EXPERIENCE|SPECIFIC TECHNICAL SKILLS|TECHNICAL SKILLS|RESEARCH ABILITY|ABILITY TO TEACH THE LANGUAGE|ABILITY TO TEACH IN THE LANGUAGE|KNOWLEDGE OF FIELD|GOOD UNDERSTANDING|FIELDWORK PROJECTS')] <- 'Specific skills, knowledge, or experience'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'RESEARCH PROPOSAL STRENGTH|VISION|FITNESS|AMBITION|RIGOUR')] <- 'Research proposal strength and vision'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'BEING A WHITE MALE|GENDER|DIVERSITY|PERSONAL BIAS*|CHILD CARE|CITIZIENSHIP|CITIZENSHIP|SOCIAL JUSTICE')] <- 'Diversity in personal demographics (need to reverse response for citizenship)'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'EXTERNAL ACADEMICS|STUDIED PREVIOUSLY AT OXFORD')] <- 'Being an internal or external candidate (both direction)'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'ECONOMIC VALUE|INDUSTRIAL/NON-ACADEMIC|PRACTICAL APPLICATIONS')] <- 'Industrial impact'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'INTER-DISIPLINARITY|INTERDISCIPLINARITY')] <- 'Interdisciplinarity'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'PEER-REVIEWING')] <- 'Peer-reviewing'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'COMMUNICATION SKILLS|PERFORMANCE IN THE INTERVIEW')] <- 'Communication skill, performance during interview'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'CONFERENCE')] <- 'Conference speaking'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'RESPECTFUL ENGAGEMENT WITH RESEARCH COMMUNITIES')] <- 'Respectful engagement with community researched'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'MOTIVATION')] <- 'Motivation'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'TEAM WORK RECORD')] <- 'Collaborative skills'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'MEDICAL IMPACT')] <- 'Medical impact'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'AVAILABILITY OF TEACHING IN PREVIOUS CAREER')] <- 'Availability of teaching in previous career'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'REPLICATION')] <- 'Number of successful replications'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'BUTTONS|PARKING|DIFFICULT TO EVALUATE')] <- 'Not categorised'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'COMMITMENT TO INCLUSIVITY|EQUITABLE RESEARCH|KINDNESS')] <- 'Commitment to inclusivity'
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(Alldata_OtherFutureRecruitment$FutureRecruitment_Other, 'LIFE EXPERIENCE|ROLES OUTSIDE')] <- 'Commitment to inclusivity'


Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode[!is.na(Alldata_OtherFutureRecruitment$FutureRecruitment_Other) & is.na(Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode)] <- 'Not categorised'
Alldata_OtherFutureRecruitment[!is.na(Alldata_OtherFutureRecruitment$FutureRecruitment_Other) & Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode == 'Not categorised',]
Alldata_OtherFutureRecruitment$FutureRecruitment_Other_score <- factor(Alldata_OtherFutureRecruitment$FutureRecruitment_Other_score , levels = Criteria_answers)

#View(Alldata_OtherFutureRecruitment[,c('FutureRecruitment_Other','FutureRecruitment_Other_recode', 'FutureRecruitment_Other_score')])

quote_Recruitement_2 <- data$FutureRecruitment_Other1[!is.na(data$FutureRecruitment_Other1) & startsWith(data$FutureRecruitment_Other1, "Commitment to inclusivity")==TRUE]
quote_Recruitement_2_source <- c(as.character(data$Role[!is.na(data$FutureRecruitment_Other1) & data$FutureRecruitment_Other1 == quote_Recruitement_2]), 
                                 data$Div[!is.na(data$FutureRecruitment_Other1) & data$FutureRecruitment_Other1 == quote_Recruitement_2])



## split per subdataset

xtab_OtherFutureRecruitment <- Alldata_OtherFutureRecruitment[Alldata_OtherFutureRecruitment$FutureRecruitment_Other_recode != 'Not categorised',]  %>% 
  tabyl(FutureRecruitment_Other_recode, FutureRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(xtab_OtherFutureRecruitment)[1] <- "Other desired future recruitment criteria" 
xtab_OtherFutureRecruitment



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
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'BEING MATES|REFERENCE|CONNECTION|WHO YOU KNOW|KNOWING PEOPLE|NETWORK*|ALUMNI|AWARDING INSTITUTION|PRESTIGE OF PAST INSTITUTION|REFEREES|REPUTATION OF FORMER SUPERVISOR|WITH A SENIOR MEMBER OF THE DEPARTMENT')] <- 'Personal connections to hiring department and prestige of former connections'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'SOCIAL MEDIA|ALTMETRICS')] <- 'Social media presence'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'PUBLIC ENGAGEMENT|OUTREACH')] <- 'Public engagement and outreach' # need this one to be below social media to overwrite one entry
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'INNOVATION|UNDERSERVED|NOVELTY|INTERESTS IN COMMON|COMPLEMENTARITY|ORIGINALITY|RESEARCH MOTIVATION AND INTEREST|IDEAS')] <- 'Having novel, underserved, or complementary expertise from the department'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'PAST EXPERIENCE|SPECIFIC TECHNICAL SKILLS|TECHNICAL SKILLS|RESEARCH ABILITY|ABILITY TO TEACH THE LANGUAGE|KNOWLEDGE OF FIELD|GOOD UNDERSTANDING|FIELDWORK PROJECTS')] <- 'Specific skills, knowledge, or experience'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'RESEARCH PROPOSAL STRENGTH|VISION|FITNESS|AMBITION|RIGOUR')] <- 'Research proposal strength and vision'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'BEING A WHITE MALE|GENDER|DIVERSITY|PERSONAL BIASES|CHILD CARE|CITIZIENSHIP|CITIZENSHIP|SOCIAL JUSTICE')] <- 'Personal demographics (both directions; e.g. gender, citizenship)'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'EXTERNAL ACADEMICS|STUDIED PREVIOUSLY AT OXFORD')] <- 'Being an internal or external candidate (both direction)'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'ECONOMIC VALUE|INDUSTRIAL/NON-ACADEMIC|PRACTICAL APPLICATIONS')] <- 'Industrial impact'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'INTER-DISIPLINARITY|INTERDISCIPLINARITY')] <- 'Interdisciplinarity'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'PEER-REVIEWING')] <- 'Peer-reviewing'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'COMMUNICATION SKILLS|PERFORMANCE IN THE INTERVIEW')] <- 'Communication skill, performance during interview'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'CONFERENCE')] <- 'Conference speaking'
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'RESPECTFUL ENGAGEMENT WITH RESEARCH COMMUNITIES')] <- 'Respectful engagement with community researched'

Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[!is.na(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other) & is.na(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode)] <- 'Not categorised'
Alldata_OtherCurrentRecruitment[!is.na(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other) & Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode == 'Not categorised',]
Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_score <- factor(Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_score , levels = Criteria_answers)

#View(Alldata_OtherCurrentRecruitment[,c('CurrentRecruitment_Other','CurrentRecruitment_Other_recode', 'CurrentRecruitment_Other_score')])

quote_Recruitement_1 <- data$CurrentRecruitment_Other1[!is.na(data$CurrentRecruitment_Other1) & startsWith(data$CurrentRecruitment_Other1, "Gender (most departments")==TRUE]
quote_Recruitement_1_source <- c(as.character(data$Role[!is.na(data$CurrentRecruitment_Other1) & data$CurrentRecruitment_Other1 == quote_Recruitement_1]), 
                                 data$Div[!is.na(data$CurrentRecruitment_Other1) & data$CurrentRecruitment_Other1 == quote_Recruitement_1])

## split per subdataset

xtab_OtherCurrentRecruitment <- Alldata_OtherCurrentRecruitment[Alldata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode != 'Not categorised',]  %>% 
  tabyl(CurrentRecruitment_Other_recode, CurrentRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(xtab_OtherCurrentRecruitment)[1] <- "Other perceived current recruitment criteria" 
xtab_OtherCurrentRecruitment

pgrdata_xtab_OtherCurrentRecruitment <- Alldata_OtherCurrentRecruitment[Alldata_OtherCurrentRecruitment$Subdataset == 'pgrdata',] %>% 
  tabyl(CurrentRecruitment_Other_recode, CurrentRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(pgrdata_xtab_OtherCurrentRecruitment)[1] <- "" 
pgrdata_xtab_OtherCurrentRecruitment


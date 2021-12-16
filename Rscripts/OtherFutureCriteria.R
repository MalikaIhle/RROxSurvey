
  #source("Rscripts/FormatData.R")

FutureRecruitment_answers <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")

# pgrdata_OtherFutureRecruitment
## subset and reformat data
pgrdata_OtherFutureRecruitment <- subset_columns_by_pattern(pgrdata, "^FutureRecruitment_Other")
pgrdata_OtherFutureRecruitment <- pgrdata_OtherFutureRecruitment[rowSums(!is.na(pgrdata_OtherFutureRecruitment)) > 1, ]
colnames(pgrdata_OtherFutureRecruitment) <- c('Div', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other','FutureRecruitment_Other_score', 'FutureRecruitment_Other')
pgrdata_OtherFutureRecruitment <- rbind(pgrdata_OtherFutureRecruitment[,c(1,2,3)], pgrdata_OtherFutureRecruitment[,c(1,4,5)], pgrdata_OtherFutureRecruitment[, c(1,6,7)])
pgrdata_OtherFutureRecruitment <- pgrdata_OtherFutureRecruitment[!is.na(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other),] 
### Nb of respondents
nrow(pgrdata_OtherFutureRecruitment)

## recode
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'references|connection|Who you know|Connections|Knowing people')] <- 'Personal connections (e.g. people from the hiring department)'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'Public engagement|Social media')] <- 'Public engagement'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'Diversity|Social Justice')] <- 'Diversity'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'fieldwork projects')] <- 'Leading major fieldwork projects'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'Innovation|Originality')] <- 'Originality of research'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'Communication skills')] <- 'Communication skills (explaining their research)'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'medical impact')] <- 'Medical impact'
pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_recode[str_detect(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other, 'Availability of teaching in previous career')] <- 'Availability of teaching in previous career'

pgrdata_OtherFutureRecruitment[,c('Div','FutureRecruitment_Other_score','FutureRecruitment_Other_recode')]

pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_score <- factor(pgrdata_OtherFutureRecruitment$FutureRecruitment_Other_score , levels = FutureRecruitment_answers)

xtab_OtherFutureRecruitment <- pgrdata_OtherFutureRecruitment %>% 
  tabyl(FutureRecruitment_Other_recode, FutureRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(xtab_OtherFutureRecruitment)[1] <- "" 

# allstaffdata_OtherFutureRecruitment 
## subset and reformat data
allstaffdata_OtherFutureRecruitment <- subset_columns_by_pattern(allstaffdata, "^FutureRecruitment_Other")
allstaffdata_OtherFutureRecruitment <- allstaffdata_OtherFutureRecruitment[rowSums(!is.na(allstaffdata_OtherFutureRecruitment)) > 1, ]
colnames(allstaffdata_OtherFutureRecruitment) <- c('Div', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other','FutureRecruitment_Other_score', 'FutureRecruitment_Other')
allstaffdata_OtherFutureRecruitment <- rbind(allstaffdata_OtherFutureRecruitment[,c(1,2,3)], allstaffdata_OtherFutureRecruitment[,c(1,4,5)], allstaffdata_OtherFutureRecruitment[, c(1,6,7)])
allstaffdata_OtherFutureRecruitment <- allstaffdata_OtherFutureRecruitment[!is.na(allstaffdata_OtherFutureRecruitment$FutureRecruitment_Other),] 
### Nb of respondents
nrow(allstaffdata_OtherFutureRecruitment)

allstaffdata_OtherFutureRecruitment

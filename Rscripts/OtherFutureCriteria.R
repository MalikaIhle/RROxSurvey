
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

# staffdata_OtherFutureRecruitment 
## subset and reformat data
staffdata_OtherFutureRecruitment <- subset_columns_by_pattern(staffdata, "^FutureRecruitment_Other")
staffdata_OtherFutureRecruitment <- staffdata_OtherFutureRecruitment[rowSums(!is.na(staffdata_OtherFutureRecruitment)) > 1, ]
colnames(staffdata_OtherFutureRecruitment) <- c('Div', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other', 'FutureRecruitment_Other_score', 'FutureRecruitment_Other','FutureRecruitment_Other_score', 'FutureRecruitment_Other')
staffdata_OtherFutureRecruitment <- rbind(staffdata_OtherFutureRecruitment[,c(1,2,3)], staffdata_OtherFutureRecruitment[,c(1,4,5)], staffdata_OtherFutureRecruitment[, c(1,6,7)])
staffdata_OtherFutureRecruitment <- staffdata_OtherFutureRecruitment[!is.na(staffdata_OtherFutureRecruitment$FutureRecruitment_Other),] 
### Nb of respondents
nrow(staffdata_OtherFutureRecruitment)

staffdata_OtherFutureRecruitment

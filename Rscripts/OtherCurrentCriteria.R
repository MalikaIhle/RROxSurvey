
  #source("Rscripts/FormatData.R")

CurrentRecruitment_answers <- c("Considerably", "Moderately", "Slightly", "Not at all","Not sure", "Not applicable")

# pgrdata_OtherCurrentRecruitment
## subset and reformat data
pgrdata_OtherCurrentRecruitment <- subset_columns_by_pattern(pgrdata, "^CurrentRecruitment_Other")
pgrdata_OtherCurrentRecruitment <- pgrdata_OtherCurrentRecruitment[rowSums(!is.na(pgrdata_OtherCurrentRecruitment)) > 1, ]
colnames(pgrdata_OtherCurrentRecruitment) <- c('Div', 'CurrentRecruitment_Other_score', 'CurrentRecruitment_Other', 'CurrentRecruitment_Other_score', 'CurrentRecruitment_Other','CurrentRecruitment_Other_score', 'CurrentRecruitment_Other')
pgrdata_OtherCurrentRecruitment <- rbind(pgrdata_OtherCurrentRecruitment[,c(1,2,3)], pgrdata_OtherCurrentRecruitment[,c(1,4,5)], pgrdata_OtherCurrentRecruitment[, c(1,6,7)])
pgrdata_OtherCurrentRecruitment <- pgrdata_OtherCurrentRecruitment[!is.na(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other),] 
### Nb of respondents
nrow(pgrdata_OtherCurrentRecruitment)

## recode
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'references|connection|Who you know|Connections|Knowing people')] <- 'Personal connections (e.g. academic references, people from the hiring department'
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Public engagement|Social media')] <- 'Public engagement and social media presence'
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Diversity')] <- 'Diversity'
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Leading major fieldwork projects')] <- 'Leading major fieldwork projects'
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'underserved')] <- 'Having expertise underserved in the department'
pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_recode[str_detect(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other, 'Academic Background')] <- 'Academic Background (not sure what is meant here)'

pgrdata_OtherCurrentRecruitment[,c('Div','CurrentRecruitment_Other_score','CurrentRecruitment_Other_recode')]

pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_score <- factor(pgrdata_OtherCurrentRecruitment$CurrentRecruitment_Other_score , levels = Criteria_answers)

xtab_OtherCurrentRecruitment <- pgrdata_OtherCurrentRecruitment %>% 
  tabyl(CurrentRecruitment_Other_recode, CurrentRecruitment_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Considerably)
names(xtab_OtherCurrentRecruitment)[1] <- "" 

# allstaffdata_OtherCurrentRecruitment
## subset and reformat data
allstaffdata_OtherCurrentRecruitment <- subset_columns_by_pattern(allstaffdata, "^CurrentRecruitment_Other")
allstaffdata_OtherCurrentRecruitment <- allstaffdata_OtherCurrentRecruitment[rowSums(!is.na(allstaffdata_OtherCurrentRecruitment)) > 1, ]
colnames(allstaffdata_OtherCurrentRecruitment) <- c('Div', 'CurrentRecruitment_Other_score', 'CurrentRecruitment_Other', 'CurrentRecruitment_Other_score', 'CurrentRecruitment_Other','CurrentRecruitment_Other_score', 'CurrentRecruitment_Other')
allstaffdata_OtherCurrentRecruitment <- rbind(allstaffdata_OtherCurrentRecruitment[,c(1,2,3)], allstaffdata_OtherCurrentRecruitment[,c(1,4,5)], allstaffdata_OtherCurrentRecruitment[, c(1,6,7)])
allstaffdata_OtherCurrentRecruitment <- allstaffdata_OtherCurrentRecruitment[!is.na(allstaffdata_OtherCurrentRecruitment$CurrentRecruitment_Other),] 
### Nb of respondents
nrow(allstaffdata_OtherCurrentRecruitment)

allstaffdata_OtherCurrentRecruitment

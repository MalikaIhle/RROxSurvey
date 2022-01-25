
  #source("Rscripts/FormatData.R")

Support_answers <- c("Essential", "Useful", "Not sure", "Not useful")

# Alldata_OtherSupport 
## subset, merge, and reformat data
pgrdata_OtherSupport <- cbind(subset_columns_by_pattern(pgrdata, "^Support_Other"), Subdataset = 'pgrdata')
staffdata_OtherSupport <- cbind(subset_columns_by_pattern(staffdata, "^Support_Other"), Subdataset = 'staffdata')
supportstaffdata_OtherSupport <- cbind(subset_columns_by_pattern(supportstaffdata, "^Support_Other"), Subdataset = 'supportstaffdata')
academicdata_OtherSupport <- cbind(subset_columns_by_pattern(academicdata, "^Support_Other"), Subdataset = 'academicdata')

Alldata_OtherSupport <- rbind(pgrdata_OtherSupport, staffdata_OtherSupport, supportstaffdata_OtherSupport, academicdata_OtherSupport)
Alldata_OtherSupport <- Alldata_OtherSupport[rowSums(!is.na(Alldata_OtherSupport)) > 2, ]
colnames(Alldata_OtherSupport) <- c('Div', 'Support_Other_score', 'Support_Other', 'Support_Other_score', 'Support_Other','Support_Other_score', 'Support_Other', 'Subdataset')
Alldata_OtherSupport <- rbind(Alldata_OtherSupport[,c(1,2,3,8)], Alldata_OtherSupport[,c(1,4,5,8)], Alldata_OtherSupport[, c(1,6,7,8)])
Alldata_OtherSupport <- Alldata_OtherSupport[!is.na(Alldata_OtherSupport$Support_Other),]
Alldata_OtherSupport$Support_Other <- toupper(Alldata_OtherSupport$Support_Other)
### Nb of respondents
nrow(Alldata_OtherSupport)

## recode
Alldata_OtherSupport$Support_Other_recode[str_detect(Alldata_OtherSupport$Support_Other, 'FUNDING|MONEY|MORE STAFF')] <- 'Funding (for OA fees, for more staff, or in general)'
Alldata_OtherSupport$Support_Other_recode[str_detect(Alldata_OtherSupport$Support_Other, 'INCENTIVE|RECRUITMENT|UNIVERSITY')] <- 'University endorsement, recruitment criteria, and  policies'
Alldata_OtherSupport$Support_Other_recode[str_detect(Alldata_OtherSupport$Support_Other, 'COURSE')] <- 'Graduate course'
Alldata_OtherSupport$Support_Other_recode[str_detect(Alldata_OtherSupport$Support_Other, 'FORCING ALL JOURNALS|LEADERSHIP')] <- 'Advocacy work with stakeholders (journals, funders, societies)'
Alldata_OtherSupport$Support_Other_recode[str_detect(Alldata_OtherSupport$Support_Other, 'DPHIL REQUIREMENTS')] <- 'DPhil requirements'
Alldata_OtherSupport$Support_Other_recode[str_detect(Alldata_OtherSupport$Support_Other, 'ONLINE COLLATION OF REPOSITORIES')] <- 'Online collation of repositories'
Alldata_OtherSupport$Support_Other_recode[str_detect(Alldata_OtherSupport$Support_Other, 'MORE LOCAL')] <- 'Departmental level support'

Alldata_OtherSupport$Support_Other_recode[!is.na(Alldata_OtherSupport$Support_Other) & is.na(Alldata_OtherSupport$Support_Other_recode)] <- 'Not categorised'
Alldata_OtherSupport[!is.na(Alldata_OtherSupport$Support_Other) & Alldata_OtherSupport$Support_Other_recode == 'Not categorised',]
Alldata_OtherSupport$Support_Other_score <- factor(Alldata_OtherSupport$Support_Other_score , levels = Support_answers)

## split per subdataset
xtab_OtherSupport <- Alldata_OtherSupport[Alldata_OtherSupport$Support_Other_recode != 'Not categorised',] %>% 
  tabyl(Support_Other_recode, Support_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Essential)
names(xtab_OtherSupport)[1] <- "" 
xtab_OtherSupport

pgrdata_xtab_OtherSupport <- Alldata_OtherSupport[Alldata_OtherSupport$Subdataset == 'pgrdata',] %>% 
  tabyl(Support_Other_recode, Support_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Essential)
names(pgrdata_xtab_OtherSupport)[1] <- "" 
pgrdata_xtab_OtherSupport


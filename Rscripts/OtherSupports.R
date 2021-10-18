
  #source("Rscripts/FormatData.R")

Support_answers <- c("Essential", "Useful", "Not sure", "Not useful")

# pgrdata_OtherSupport 
## subset and reformat data
pgrdata_OtherSupport <- subset_columns_by_pattern(pgrdata, "^Support_Other")
pgrdata_OtherSupport <- pgrdata_OtherSupport[rowSums(!is.na(pgrdata_OtherSupport)) > 1, ]
colnames(pgrdata_OtherSupport) <- c('Div', 'Support_Other_score', 'Support_Other', 'Support_Other_score', 'Support_Other','Support_Other_score', 'Support_Other')
pgrdata_OtherSupport <- rbind(pgrdata_OtherSupport[,c(1,2,3)], pgrdata_OtherSupport[,c(1,4,5)], pgrdata_OtherSupport[, c(1,6,7)])
pgrdata_OtherSupport <- pgrdata_OtherSupport[!is.na(pgrdata_OtherSupport$Support_Other),]
### Nb of respondents
nrow(pgrdata_OtherSupport)

## recode
pgrdata_OtherSupport$Support_Other_recode[str_detect(pgrdata_OtherSupport$Support_Other, 'Funding')] <- 'Funding'
pgrdata_OtherSupport$Support_Other_recode[str_detect(pgrdata_OtherSupport$Support_Other, 'find funding')] <- 'Information on how to find funding for publishing in hybrid journal'
pgrdata_OtherSupport$Support_Other_recode[str_detect(pgrdata_OtherSupport$Support_Other, 'incentives|recruitment practices|university')] <- 'University endorsement, recruitement criteria, and  policies'
pgrdata_OtherSupport$Support_Other_recode[str_detect(pgrdata_OtherSupport$Support_Other, 'Publishing|publishing')] <- 'Training on how to publish transparent research'

pgrdata_OtherSupport[,c('Div','Support_Other_score', 'Support_Other_recode')]

pgrdata_OtherSupport$Support_Other_score <- factor(pgrdata_OtherSupport$Support_Other_score , levels = Support_answers)

xtab_OtherSupport <- pgrdata_OtherSupport %>% 
  tabyl(Support_Other_recode, Support_Other_score, show_missing_levels = FALSE) %>% 
  arrange(-Essential)
names(xtab_OtherSupport)[1] <- "" 


# staffdata_OtherSupport 
## subset and reformat data
staffdata_OtherSupport <- subset_columns_by_pattern(staffdata, "^Support_Other")
staffdata_OtherSupport <- staffdata_OtherSupport[rowSums(!is.na(staffdata_OtherSupport)) > 1, ]
colnames(staffdata_OtherSupport) <- c('Div', 'Support_Other_score', 'Support_Other', 'Support_Other_score', 'Support_Other','Support_Other_score', 'Support_Other')
staffdata_OtherSupport <- rbind(staffdata_OtherSupport[,c(1,2,3)], staffdata_OtherSupport[,c(1,4,5)], staffdata_OtherSupport[, c(1,6,7)])
staffdata_OtherSupport <- staffdata_OtherSupport[!is.na(staffdata_OtherSupport$Support_Other),]
### Nb of respondents
nrow(staffdata_OtherSupport)

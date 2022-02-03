
  #source("Rscripts/FormatData.R")

# Suring data collection: Descriptive statistics: Numbers of responses per Dpt, survey duration, experience duration
## Check coverage across department
pt <- data.frame(data_round2 %>% group_by(Div, Role, Dept) %>% summarise(n = n()))
# write.csv(pt,"Dept20220202perroles.csv", row.names = FALSE)


## Number of years of experience (not used later on - so far)
summary(data_round2$Duration)
data.frame(data_round2 %>% group_by(Div) %>% summarise(minDuration = min(Duration, na.rm=TRUE),
                                                   medDuration = median(Duration, na.rm=TRUE),
                                                   meanDuration = mean(Duration, na.rm=TRUE),
                                                   maxDuration = max(Duration, na.rm=TRUE),
                                                   n = n(),
                                                   NADuration = sum(is.na(Duration))))
# Sample sizes 
## sample sizes by questions (is.not NA)
### for pgr data
pgrdata_Consent_Affiliation_Role_ss <- pgrdata %>% group_by(Div) %>% summarise (Consent_Affiliation_Role=n())
pgrdata_Duration_ss <- sample_size_perQ(pgrdata_Duration, ExperienceDuration)
pgrdata_Awareness_ss <- sample_size_perQ(pgrdata_Awareness, Awareness)
pgrdata_Effect_ss <- sample_size_perQ(pgrdata_Effect, Effect)
pgrdata_Barriers_ss <- sample_size_perQ(pgrdata_Barriers, Barriers)
pgrdata_Downsides_ss <- sample_size_perQ(pgrdata_Downsides, Downsides)
pgrdata_CurrentRecruitment_ss <- sample_size_perQ(pgrdata_CurrentRecruitment, CurrentRecruitment)
pgrdata_FutureRecruitment_ss <- sample_size_perQ(pgrdata_FutureRecruitment, FutureRecruitment)
pgrdata_Training_ss <- sample_size_perQ(pgrdata_Training, Training)
pgrdata_Support_ss <- sample_size_perQ(pgrdata_Support, Support)

### for allstaff data
allstaffdata_Consent_Affiliation_Role_ss <- allstaffdata %>% group_by(Div) %>% summarise (Consent_Affiliation_Role=n())
allstaffdata_Duration <- allstaffdata[, c( "Div", "Duration")]
allstaffdata_Duration_ss <- sample_size_perQ(allstaffdata_Duration, ExperienceDuration)
allstaffdata_Awareness_ss <- sample_size_perQ(allstaffdata_Awareness, Awareness)
allstaffdata_Effect_ss <- sample_size_perQ(allstaffdata_Effect, Effect)
allstaffdata_Barriers_ss <- sample_size_perQ(allstaffdata_Barriers, Barriers)
allstaffdata_Downsides_ss <- sample_size_perQ(allstaffdata_Downsides, Downsides)
allstaffdata_CurrentRecruitment_ss <- sample_size_perQ(allstaffdata_CurrentRecruitment, CurrentRecruitment)
allstaffdata_FutureRecruitment_ss <- sample_size_perQ(allstaffdata_FutureRecruitment, FutureRecruitment)
allstaffdata_Training_ss <- sample_size_perQ(allstaffdata_Training, Training)
allstaffdata_Support_ss <- sample_size_perQ(allstaffdata_Support, Support)

### for staff data
staffdata_Consent_Affiliation_Role_ss <- staffdata %>% group_by(Div) %>% summarise (Consent_Affiliation_Role=n())
staffdata_Duration <- staffdata[, c( "Div", "Duration")]
staffdata_Duration_ss <- sample_size_perQ(staffdata_Duration, ExperienceDuration)
staffdata_Awareness_ss <- sample_size_perQ(staffdata_Awareness, Awareness)
staffdata_Effect_ss <- sample_size_perQ(staffdata_Effect, Effect)
staffdata_Barriers_ss <- sample_size_perQ(staffdata_Barriers, Barriers)
staffdata_Downsides_ss <- sample_size_perQ(staffdata_Downsides, Downsides)
staffdata_CurrentRecruitment_ss <- sample_size_perQ(staffdata_CurrentRecruitment, CurrentRecruitment)
staffdata_FutureRecruitment_ss <- sample_size_perQ(staffdata_FutureRecruitment, FutureRecruitment)
staffdata_Training_ss <- sample_size_perQ(staffdata_Training, Training)
staffdata_Support_ss <- sample_size_perQ(staffdata_Support, Support)

### for support staff data
supportstaffdata_Consent_Affiliation_Role_ss <- supportstaffdata %>% group_by(Div) %>% summarise (Consent_Affiliation_Role=n())
supportstaffdata_Duration <- supportstaffdata[, c( "Div", "Duration")]
supportstaffdata_Duration_ss <- sample_size_perQ(supportstaffdata_Duration, ExperienceDuration)
supportstaffdata_Awareness_ss <- sample_size_perQ(supportstaffdata_Awareness, Awareness)
supportstaffdata_Effect_ss <- sample_size_perQ(supportstaffdata_Effect, Effect)
supportstaffdata_Barriers_ss <- sample_size_perQ(supportstaffdata_Barriers, Barriers)
supportstaffdata_Downsides_ss <- sample_size_perQ(supportstaffdata_Downsides, Downsides)
supportstaffdata_CurrentRecruitment_ss <- sample_size_perQ(supportstaffdata_CurrentRecruitment, CurrentRecruitment)
supportstaffdata_FutureRecruitment_ss <- sample_size_perQ(supportstaffdata_FutureRecruitment, FutureRecruitment)
supportstaffdata_Training_ss <- sample_size_perQ(supportstaffdata_Training, Training)
supportstaffdata_Support_ss <- sample_size_perQ(supportstaffdata_Support, Support)


### for academic data
academicdata_Consent_Affiliation_Role_ss <- academicdata %>% group_by(Div) %>% summarise (Consent_Affiliation_Role=n())
academicdata_Duration <- academicdata[, c( "Div", "Duration")]
academicdata_Duration_ss <- sample_size_perQ(academicdata_Duration, ExperienceDuration)
academicdata_Awareness_ss <- sample_size_perQ(academicdata_Awareness, Awareness)
academicdata_Effect_ss <- sample_size_perQ(academicdata_Effect, Effect)
academicdata_Barriers_ss <- sample_size_perQ(academicdata_Barriers, Barriers)
academicdata_Downsides_ss <- sample_size_perQ(academicdata_Downsides, Downsides)
academicdata_CurrentRecruitment_ss <- sample_size_perQ(academicdata_CurrentRecruitment, CurrentRecruitment)
academicdata_FutureRecruitment_ss <- sample_size_perQ(academicdata_FutureRecruitment, FutureRecruitment)
academicdata_Training_ss <- sample_size_perQ(academicdata_Training, Training)
academicdata_Support_ss <- sample_size_perQ(academicdata_Support, Support)

## prepare table with sample sizes for pgrdata
ss_pgrdata <- pgrdata_Consent_Affiliation_Role_ss %>%
  full_join(pgrdata_Awareness_ss,  by = 'Div') %>% 
  full_join(pgrdata_Duration_ss,  by = 'Div') %>% 
  full_join(pgrdata_Effect_ss,  by = 'Div') %>% 
  full_join(pgrdata_Barriers_ss,  by = 'Div') %>% 
  full_join(pgrdata_Downsides_ss,  by = 'Div') %>% 
  full_join(pgrdata_CurrentRecruitment_ss,  by = 'Div') %>% 
  full_join(pgrdata_FutureRecruitment_ss,  by = 'Div') %>% 
  full_join(pgrdata_Training_ss,  by = 'Div') %>% 
  full_join(pgrdata_Support_ss,  by = 'Div') %>% 
  full_join(targetnumbers[,c('Div', 'StudentTotal2021')],  by = 'Div') 

ss_pgrdata <- rbind(ss_pgrdata, c("Total", colSums(ss_pgrdata[,-1], na.rm = TRUE)))
ss_pgrdata <- ss_pgrdata[!ss_pgrdata$StudentTotal2021 == 0,] # remove GLAM since no students

ss_pgrdata$PercRepresentativeness <- round(as.numeric(ss_pgrdata$Consent_Affiliation_Role)*100/as.numeric(ss_pgrdata$StudentTotal2021),1)
ss_pgrdata[,2:ncol(ss_pgrdata)] <- sapply(ss_pgrdata[,2:ncol(ss_pgrdata)], as.integer) # needed for the apply function to work
ss_pgrdata$TotalDrop <- apply(ss_pgrdata[,2:10], 1, max) - apply(ss_pgrdata[,2:10], 1, min)
ss_pgrdata$PercDrop <- round(ss_pgrdata$TotalDrop/apply(ss_pgrdata[,2:10], 1, max)*100,1)

ss_pgrdata <- data.frame(ss_pgrdata)
sst_pgrdata <- transpose(ss_pgrdata)
colnames(sst_pgrdata) <- sst_pgrdata[1,]
sst_pgrdata <- sst_pgrdata[-1,]
sst_pgrdata$Question <- colnames(ss_pgrdata)[-1]
rownames(sst_pgrdata) <- NULL
sst_pgrdata <- sst_pgrdata[,c(ncol(sst_pgrdata), 1:(ncol(sst_pgrdata)-1))]
sst_pgrdata


## prepare table with sample sizes for allstaffdata
ss_allstaffdata <- allstaffdata_Consent_Affiliation_Role_ss %>%
  full_join(allstaffdata_Awareness_ss,  by = 'Div') %>% 
  full_join(allstaffdata_Duration_ss,  by = 'Div') %>% 
  full_join(allstaffdata_Effect_ss,  by = 'Div') %>% 
  full_join(allstaffdata_Barriers_ss,  by = 'Div') %>% 
  full_join(allstaffdata_Downsides_ss,  by = 'Div') %>% 
  full_join(allstaffdata_CurrentRecruitment_ss,  by = 'Div') %>% 
  full_join(allstaffdata_FutureRecruitment_ss,  by = 'Div') %>% 
  full_join(allstaffdata_Training_ss,  by = 'Div') %>% 
  full_join(allstaffdata_Support_ss,  by = 'Div') %>% 
  full_join(targetnumbers[,c('Div', 'AllStaffTotal2019')],  by = 'Div') ### need updating !!

ss_allstaffdata <- rbind(ss_allstaffdata, c("Total", colSums(ss_allstaffdata[,-1], na.rm = TRUE)))

ss_allstaffdata$PercRepresentativeness <- round(as.numeric(ss_allstaffdata$Consent_Affiliation_Role)*100/as.numeric(ss_allstaffdata$AllStaffTotal2019),1)
ss_allstaffdata[,2:ncol(ss_allstaffdata)] <- sapply(ss_allstaffdata[,2:ncol(ss_allstaffdata)], as.integer) # needed for the apply function to work
ss_allstaffdata$TotalDrop <- apply(ss_allstaffdata[,2:10], 1, max) - apply(ss_allstaffdata[,2:10], 1, min)
ss_allstaffdata$PercDrop <- round(ss_allstaffdata$TotalDrop/apply(ss_allstaffdata[,2:10], 1, max)*100,1)

ss_allstaffdata <- data.frame(ss_allstaffdata)
sst_allstaffdata <- transpose(ss_allstaffdata)
colnames(sst_allstaffdata) <- sst_allstaffdata[1,]
sst_allstaffdata <- sst_allstaffdata[-1,]
sst_allstaffdata$Question <- colnames(ss_allstaffdata)[-1]
rownames(sst_allstaffdata) <- NULL
sst_allstaffdata <- sst_allstaffdata[,c(ncol(sst_allstaffdata), 1:(ncol(sst_allstaffdata)-1))]
sst_allstaffdata


## prepare table with sample sizes for staffdata
ss_staffdata <- staffdata_Consent_Affiliation_Role_ss %>%
  full_join(staffdata_Awareness_ss,  by = 'Div') %>% 
  full_join(staffdata_Duration_ss,  by = 'Div') %>% 
  full_join(staffdata_Effect_ss,  by = 'Div') %>% 
  full_join(staffdata_Barriers_ss,  by = 'Div') %>% 
  full_join(staffdata_Downsides_ss,  by = 'Div') %>% 
  full_join(staffdata_CurrentRecruitment_ss,  by = 'Div') %>% 
  full_join(staffdata_FutureRecruitment_ss,  by = 'Div') %>% 
  full_join(staffdata_Training_ss,  by = 'Div') %>% 
  full_join(staffdata_Support_ss,  by = 'Div') %>% 
  full_join(targetnumbers[,c('Div', 'ResearchStaffTotal2020')],  by = 'Div') ### need updating !!

ss_staffdata <- rbind(ss_staffdata, c("Total", colSums(ss_staffdata[,-1], na.rm = TRUE)))
ss_staffdata <- ss_staffdata[!ss_staffdata$ResearchStaffTotal2020 == 0,] # remove ContEd since no staff

ss_staffdata$PercRepresentativeness <- round(as.numeric(ss_staffdata$Consent_Affiliation_Role)*100/as.numeric(ss_staffdata$ResearchStaffTotal2020),1)
ss_staffdata[,2:ncol(ss_staffdata)] <- sapply(ss_staffdata[,2:ncol(ss_staffdata)], as.integer) # needed for the apply function to work
ss_staffdata$TotalDrop <- apply(ss_staffdata[,2:10], 1, max) - apply(ss_staffdata[,2:10], 1, min)
ss_staffdata$PercDrop <- round(ss_staffdata$TotalDrop/apply(ss_staffdata[,2:10], 1, max)*100,1)

ss_staffdata <- data.frame(ss_staffdata)
sst_staffdata <- transpose(ss_staffdata)
colnames(sst_staffdata) <- sst_staffdata[1,]
sst_staffdata <- sst_staffdata[-1,]
sst_staffdata$Question <- colnames(ss_staffdata)[-1]
rownames(sst_staffdata) <- NULL
sst_staffdata <- sst_staffdata[,c(ncol(sst_staffdata), 1:(ncol(sst_staffdata)-1))]
sst_staffdata


## prepare table with sample sizes for support staff data
ss_supportstaffdata <- supportstaffdata_Consent_Affiliation_Role_ss %>%
  full_join(supportstaffdata_Awareness_ss,  by = 'Div') %>% 
  full_join(supportstaffdata_Duration_ss,  by = 'Div') %>% 
  full_join(supportstaffdata_Effect_ss,  by = 'Div') %>% 
  full_join(supportstaffdata_Barriers_ss,  by = 'Div') %>% 
  full_join(supportstaffdata_Downsides_ss,  by = 'Div') %>% 
  full_join(supportstaffdata_CurrentRecruitment_ss,  by = 'Div') %>% 
  full_join(supportstaffdata_FutureRecruitment_ss,  by = 'Div') %>% 
  full_join(supportstaffdata_Training_ss,  by = 'Div') %>% 
  full_join(supportstaffdata_Support_ss,  by = 'Div') %>% 
  full_join(targetnumbers[,c('Div', 'ResearchSupportTotal2019')],  by = 'Div') ### need updating !!

ss_supportstaffdata[is.na(ss_supportstaffdata)] <- 0 # because ContEd has NA 
ss_supportstaffdata <- rbind(ss_supportstaffdata, c("Total", colSums(ss_supportstaffdata[,-1], na.rm = TRUE)))

ss_supportstaffdata$PercRepresentativeness <- round(as.numeric(ss_supportstaffdata$Consent_Affiliation_Role)*100/as.numeric(ss_supportstaffdata$ResearchSupportTotal2019),1)
ss_supportstaffdata[,2:ncol(ss_supportstaffdata)] <- sapply(ss_supportstaffdata[,2:ncol(ss_supportstaffdata)], as.integer) # needed for the apply function to work
ss_supportstaffdata$TotalDrop <- apply(ss_supportstaffdata[,2:10], 1, max) - apply(ss_supportstaffdata[,2:10], 1, min)
ss_supportstaffdata$PercDrop <- round(ss_supportstaffdata$TotalDrop/apply(ss_supportstaffdata[,2:10], 1, max)*100,1)

ss_supportstaffdata <- data.frame(ss_supportstaffdata)
sst_supportstaffdata <- transpose(ss_supportstaffdata)
colnames(sst_supportstaffdata) <- sst_supportstaffdata[1,]
sst_supportstaffdata <- sst_supportstaffdata[-1,]
sst_supportstaffdata$Question <- colnames(ss_supportstaffdata)[-1]
rownames(sst_supportstaffdata) <- NULL
sst_supportstaffdata <- sst_supportstaffdata[,c(ncol(sst_supportstaffdata), 1:(ncol(sst_supportstaffdata)-1))]
sst_supportstaffdata


## prepare table with sample sizes for academic data
ss_academicdata <- academicdata_Consent_Affiliation_Role_ss %>%
  full_join(academicdata_Awareness_ss,  by = 'Div') %>% 
  full_join(academicdata_Duration_ss,  by = 'Div') %>% 
  full_join(academicdata_Effect_ss,  by = 'Div') %>% 
  full_join(academicdata_Barriers_ss,  by = 'Div') %>% 
  full_join(academicdata_Downsides_ss,  by = 'Div') %>% 
  full_join(academicdata_CurrentRecruitment_ss,  by = 'Div') %>% 
  full_join(academicdata_FutureRecruitment_ss,  by = 'Div') %>% 
  full_join(academicdata_Training_ss,  by = 'Div') %>% 
  full_join(academicdata_Support_ss,  by = 'Div') %>% 
  full_join(targetnumbers[,c('Div', 'AcademicTotal2019')],  by = 'Div') ### need updating !!

ss_academicdata <- rbind(ss_academicdata, c("Total", colSums(ss_academicdata[,-1], na.rm = TRUE)))

ss_academicdata$PercRepresentativeness <- round(as.numeric(ss_academicdata$Consent_Affiliation_Role)*100/as.numeric(ss_academicdata$AcademicTotal2019),1)
ss_academicdata[,2:ncol(ss_academicdata)] <- sapply(ss_academicdata[,2:ncol(ss_academicdata)], as.integer) # needed for the apply function to work
ss_academicdata$TotalDrop <- apply(ss_academicdata[,2:10], 1, max) - apply(ss_academicdata[,2:10], 1, min)
ss_academicdata$PercDrop <- round(ss_academicdata$TotalDrop/apply(ss_academicdata[,2:10], 1, max)*100,1)

ss_academicdata <- data.frame(ss_academicdata)
sst_academicdata <- transpose(ss_academicdata)
colnames(sst_academicdata) <- sst_academicdata[1,]
sst_academicdata <- sst_academicdata[-1,]
sst_academicdata$Question <- colnames(ss_academicdata)[-1]
rownames(sst_academicdata) <- NULL
sst_academicdata <- sst_academicdata[,c(ncol(sst_academicdata), 1:(ncol(sst_academicdata)-1))]
sst_academicdata

## all combined
### for all data
data_Consent_Affiliation_Role_ss <- data %>% group_by(Div) %>% summarise (Consent_Affiliation_Role=n())
data_Awareness_ss <- sample_size_perQ(data_Awareness, Awareness)
data_Effect_ss <- sample_size_perQ(data_Effect, Effect)
data_Barriers_ss <- sample_size_perQ(data_Barriers, Barriers)
data_Downsides_ss <- sample_size_perQ(data_Downsides, Downsides)
data_CurrentRecruitment_ss <- sample_size_perQ(data_CurrentRecruitment, CurrentRecruitment)
data_FutureRecruitment_ss <- sample_size_perQ(data_FutureRecruitment, FutureRecruitment)
data_Training_ss <- sample_size_perQ(data_Training, Training)
data_Support_ss <- sample_size_perQ(data_Support, Support)

ss_data <- data_Consent_Affiliation_Role_ss %>%
  full_join(data_Awareness_ss,  by = 'Div') %>% 
  full_join(data_Effect_ss,  by = 'Div') %>% 
  full_join(data_Barriers_ss,  by = 'Div') %>% 
  full_join(data_Downsides_ss,  by = 'Div') %>% 
  full_join(data_CurrentRecruitment_ss,  by = 'Div') %>% 
  full_join(data_FutureRecruitment_ss,  by = 'Div') %>% 
  full_join(data_Training_ss,  by = 'Div') %>% 
  full_join(data_Support_ss,  by = 'Div') %>% 
  full_join(targetnumbers[,c('Div', 'Researchers2022')],  by = 'Div') ### need updating !!

ss_data <- rbind(ss_data, c("Total", colSums(ss_data[,-1], na.rm = TRUE)))

ss_data$PercRepresentativeness <- round(as.numeric(ss_data$Consent_Affiliation_Role)*100/as.numeric(ss_data$Researchers2022),1)
ss_data[,2:ncol(ss_data)] <- sapply(ss_data[,2:ncol(ss_data)], as.integer) # needed for the apply function to work
ss_data$TotalDrop <- apply(ss_data[,2:10], 1, max) - apply(ss_data[,2:10], 1, min)
ss_data$PercDrop <- round(ss_data$TotalDrop/apply(ss_data[,2:10], 1, max)*100,1)

ss_data <- data.frame(ss_data)
sst_data <- transpose(ss_data)
colnames(sst_data) <- sst_data[1,]
sst_data <- sst_data[-1,]
sst_data$Question <- colnames(ss_data)[-1]
rownames(sst_data) <- NULL
sst_data <- sst_data[,c(ncol(sst_data), 1:(ncol(sst_data)-1))]
sst_data


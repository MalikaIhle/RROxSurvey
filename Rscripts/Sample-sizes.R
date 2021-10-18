
  #source("Rscripts/FormatData.R")

# Descriptive statistics: Numbers of responses per Dpt, survey duration, experience duration
## during data collection - check coverage across department
data.frame(pgrdata %>% group_by(Div, Dept) %>% summarise(n = n()))

## Number of years of experience (not used later on - so far)
summary(pgrdata$Duration)
data.frame(pgrdata %>% group_by(Div) %>% summarise(minDuration = min(Duration, na.rm=TRUE),
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
  full_join(targetnumbers[,c('Div', 'StaffTotal2019')],  by = 'Div') ### need updating !!

ss_staffdata[is.na(ss_staffdata)] <- 0
ss_staffdata <- rbind(ss_staffdata, c("Total", colSums(ss_staffdata[,-1], na.rm = TRUE)))

ss_staffdata$PercRepresentativeness <- round(as.numeric(ss_staffdata$Consent_Affiliation_Role)*100/as.numeric(ss_staffdata$StaffTotal2019),1)
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


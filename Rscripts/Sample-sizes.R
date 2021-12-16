
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

ss_allstaffdata[is.na(ss_allstaffdata)] <- 0
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


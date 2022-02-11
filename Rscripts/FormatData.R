# Packages
library(here)
library(tidyverse)
library(RColorBrewer)
library(janitor)
library(egg) # for function egg:: ggarrange (next package overwrites it because when not specified in code I want ggarrange from ggpubr). this one equalise width for shared axis
library(ggpubr) # for function ggarrange for grid ggplot. this one create common legend
library(arsenal) # for function comparedf (to compare dataset created by hand and output of function)
library(data.table) # for transpose(df) 
library(reshape2) # for making pivot tables (function 'dcast' in functions and parameter .R)
library(pivottabler) # for making other pivot tables (function 'PivotTable' in sample size .R)

# Functions
source(here::here("Rscripts","Functions-and-Parameters.R"))

# Load data
data_round1 <- read.csv(here("Data/RealData_20210302-0945.csv"), stringsAsFactors=FALSE)
data_round2 <- read.csv(here("Data/RealData_20220211-1100.csv"), stringsAsFactors=FALSE)
surveyindex <- read.csv(here("Data/SurveyIndex.csv"), stringsAsFactors=FALSE)
targetnumbers <- read.csv(here("Data/TargetNumbers.csv"), stringsAsFactors=FALSE) # only has Total staff in 2019!

# Format data
data_round1 <- clean_qualtrics_data(data_round1, surveyindex)
data_round2 <- clean_qualtrics_data(data_round2, surveyindex)
data_round12 <- rbind(data_round1, data_round2) # merging both rounds
rm(surveyindex)

# Split data per respondent type
  ## will we want to bind staff data collected in both rounds or keep them separate (at least keep an ID from when they were collected - date or entry?)




data <- data_round12 # <------------------!!!!!!!!!!!!!!!!!!!!!! decide which dataset to use !!!!!!!!!!!!!!!!!!!!!





pgrdata <- data[data$StudentStaff == "Student",] # will possibly need to rbind pgrdata collected during round2
allstaffdata <- data[data$StudentStaff == "Staff",] # all research staff + support staff + academic pooled

staffdata <- data[data$Role == "Research Staff",] 
supportstaffdata <- data[data$Role == "Research Support Staff",]
academicdata <- data[data$Role == "Academic",]



# Split data per question
## Number of years of experience (not used later on so far)
summary(data$Duration)
data.frame(data %>% group_by(Div) %>% summarise(minDuration = min(Duration, na.rm=TRUE),
                                                       medDuration = median(Duration, na.rm=TRUE),
                                                       meanDuration = mean(Duration, na.rm=TRUE),
                                                       maxDuration = max(Duration, na.rm=TRUE),
                                                       n = n(),
                                                       NADuration = sum(is.na(Duration))))

## Other questions
pgrdata_Duration <- pgrdata[, c( "Div", "Duration")] # currently only used for sample size - should probably correlate this to other measures
pgrdata_Awareness <- subset_columns_by_pattern(pgrdata, "^Awareness")
pgrdata_Effect <- subset_columns_by_pattern(pgrdata, "^Effect")
pgrdata_Barriers <- subset_columns_by_pattern(pgrdata, "^Barriers")
pgrdata_Downsides <- subset_columns_by_pattern(pgrdata, "^Downsides")
pgrdata_CurrentRecruitment <- subset_columns_by_pattern(pgrdata, "^CurrentRecruitment")
pgrdata_FutureRecruitment <- subset_columns_by_pattern(pgrdata, "^FutureRecruitment")
pgrdata_Training <- subset_columns_by_pattern(pgrdata, "^Training")
pgrdata_Support <- subset_columns_by_pattern(pgrdata, "^Support")

allstaffdata_Awareness <- subset_columns_by_pattern(allstaffdata, "^Awareness")
allstaffdata_Effect <- subset_columns_by_pattern(allstaffdata, "^Effect")
allstaffdata_Barriers <- subset_columns_by_pattern(allstaffdata, "^Barriers")
allstaffdata_Downsides <- subset_columns_by_pattern(allstaffdata, "^Downsides")
allstaffdata_CurrentRecruitment <- subset_columns_by_pattern(allstaffdata, "^CurrentRecruitment")
allstaffdata_FutureRecruitment <- subset_columns_by_pattern(allstaffdata, "^FutureRecruitment")
allstaffdata_Training <- subset_columns_by_pattern(allstaffdata, "^Training")
allstaffdata_Support <- subset_columns_by_pattern(allstaffdata, "^Support")

staffdata_Awareness <- subset_columns_by_pattern(staffdata, "^Awareness")
staffdata_Effect <- subset_columns_by_pattern(staffdata, "^Effect")
staffdata_Barriers <- subset_columns_by_pattern(staffdata, "^Barriers")
staffdata_Downsides <- subset_columns_by_pattern(staffdata, "^Downsides")
staffdata_CurrentRecruitment <- subset_columns_by_pattern(staffdata, "^CurrentRecruitment")
staffdata_FutureRecruitment <- subset_columns_by_pattern(staffdata, "^FutureRecruitment")
staffdata_Training <- subset_columns_by_pattern(staffdata, "^Training")
staffdata_Support <- subset_columns_by_pattern(staffdata, "^Support")

supportstaffdata_Awareness <- subset_columns_by_pattern(supportstaffdata, "^Awareness")
supportstaffdata_Effect <- subset_columns_by_pattern(supportstaffdata, "^Effect")
supportstaffdata_Barriers <- subset_columns_by_pattern(supportstaffdata, "^Barriers")
supportstaffdata_Downsides <- subset_columns_by_pattern(supportstaffdata, "^Downsides")
supportstaffdata_CurrentRecruitment <- subset_columns_by_pattern(supportstaffdata, "^CurrentRecruitment")
supportstaffdata_FutureRecruitment <- subset_columns_by_pattern(supportstaffdata, "^FutureRecruitment")
supportstaffdata_Training <- subset_columns_by_pattern(supportstaffdata, "^Training")
supportstaffdata_Support <- subset_columns_by_pattern(supportstaffdata, "^Support")

academicdata_Awareness <- subset_columns_by_pattern(academicdata, "^Awareness")
academicdata_Effect <- subset_columns_by_pattern(academicdata, "^Effect")
academicdata_Barriers <- subset_columns_by_pattern(academicdata, "^Barriers")
academicdata_Downsides <- subset_columns_by_pattern(academicdata, "^Downsides")
academicdata_CurrentRecruitment <- subset_columns_by_pattern(academicdata, "^CurrentRecruitment")
academicdata_FutureRecruitment <- subset_columns_by_pattern(academicdata, "^FutureRecruitment")
academicdata_Training <- subset_columns_by_pattern(academicdata, "^Training")
academicdata_Support <- subset_columns_by_pattern(academicdata, "^Support")

data_Awareness <- subset_columns_by_pattern(data, "^Awareness")
data_Effect <- subset_columns_by_pattern(data, "^Effect")
data_Barriers <- subset_columns_by_pattern(data, "^Barriers")
data_Downsides <- subset_columns_by_pattern(data, "^Downsides")
data_CurrentRecruitment <- subset_columns_by_pattern(data, "^CurrentRecruitment")
data_FutureRecruitment <- subset_columns_by_pattern(data, "^FutureRecruitment")
data_Training <- subset_columns_by_pattern(data, "^Training")
data_Support <- subset_columns_by_pattern(data, "^Support")

# typos to correct 
## (doesn't work on simulated data for reasons I do not find - and if applied to data_round, it changes all columns to characters...)
pgrdata_Effect <- as.data.frame(lapply(pgrdata_Effect, gsub, pattern = "Neutral(neither detrimental nor beneficial)", 
                                         replacement = "Neutral (neither detrimental nor beneficial)", fixed = TRUE))
allstaffdata_Effect <- as.data.frame(lapply(allstaffdata_Effect, gsub, pattern = "Neutral(neither detrimental nor beneficial)", 
                             replacement = "Neutral (neither detrimental nor beneficial)", fixed = TRUE))
staffdata_Effect <- as.data.frame(lapply(staffdata_Effect, gsub, pattern = "Neutral(neither detrimental nor beneficial)", 
                                            replacement = "Neutral (neither detrimental nor beneficial)", fixed = TRUE))
supportstaffdata_Effect <- as.data.frame(lapply(supportstaffdata_Effect, gsub, pattern = "Neutral(neither detrimental nor beneficial)", 
                                            replacement = "Neutral (neither detrimental nor beneficial)", fixed = TRUE))
academicdata_Effect <- as.data.frame(lapply(academicdata_Effect, gsub, pattern = "Neutral(neither detrimental nor beneficial)", 
                                            replacement = "Neutral (neither detrimental nor beneficial)", fixed = TRUE))


source(here::here("Rscripts","Sample-sizes.R"))

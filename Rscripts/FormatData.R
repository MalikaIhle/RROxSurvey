# Packages
library(here)
library(tidyverse)
library(RColorBrewer)
library(janitor)
library(ggpubr) # for function ggarange for grid ggplot
library(arsenal) # for function comparedf (to compare dataset created by hand and output of function)
library(data.table) # for transpose(df) 
library(reshape2) # for making pivot tables

# Functions
source(here::here("Rscripts","Functions-and-Parameters.R"))

# Load data
data_round1 <- read.csv(here("Data/RealData_20210302-0945.csv"), stringsAsFactors=FALSE)
#data_round2 <- read.csv(here("Data/SimulatedData_20201214.csv"), stringsAsFactors=FALSE) ## this is simulated data to prep the code for round 2
surveyindex <- read.csv(here("Data/SurveyIndex.csv"), stringsAsFactors=FALSE)
targetnumbers <- read.csv(here("Data/TargetNumbers.csv"), stringsAsFactors=FALSE) # only has Total staff in 2019!

# Format data
data_round1 <- clean_qualtrics_data(data_round1, surveyindex)
#data_round2 <- clean_qualtrics_data(data_round2, surveyindex) ## this is simulated data to prep the code for round 2
rm(surveyindex)

# Split data per respondent type
  ## will we want to bind staff data collected in both rounds or keep them separate (at least keep an ID from when they were collected - date or entry?)
pgrdata <- data_round1[data_round1$StudentStaff == "Student",] # will possibly need to rbind pgrdata collected during round2
staffdata <- data_round1[data_round1$StudentStaff == "Staff",] #staffdata <- rbind(data_round1[data_round1$StudentStaff == "Staff",], data_round2)

# Split data per question
pgrdata_Duration <- pgrdata[, c( "Div", "Duration")] # currently only used for sample size - should probably correlate this to other measures
pgrdata_Awareness <- subset_columns_by_pattern(pgrdata, "^Awareness")
pgrdata_Effect <- subset_columns_by_pattern(pgrdata, "^Effect")
pgrdata_Barriers <- subset_columns_by_pattern(pgrdata, "^Barriers")
pgrdata_Downsides <- subset_columns_by_pattern(pgrdata, "^Downsides")
pgrdata_CurrentRecruitment <- subset_columns_by_pattern(pgrdata, "^CurrentRecruitment")
pgrdata_FutureRecruitment <- subset_columns_by_pattern(pgrdata, "^FutureRecruitment")
pgrdata_Training <- subset_columns_by_pattern(pgrdata, "^Training")
pgrdata_Support <- subset_columns_by_pattern(pgrdata, "^Support")

staffdata_Awareness <- subset_columns_by_pattern(staffdata, "^Awareness")
staffdata_Effect <- subset_columns_by_pattern(staffdata, "^Effect")
staffdata_Barriers <- subset_columns_by_pattern(staffdata, "^Barriers")
staffdata_Downsides <- subset_columns_by_pattern(staffdata, "^Downsides")
staffdata_CurrentRecruitment <- subset_columns_by_pattern(staffdata, "^CurrentRecruitment")
staffdata_FutureRecruitment <- subset_columns_by_pattern(staffdata, "^FutureRecruitment")
staffdata_Training <- subset_columns_by_pattern(staffdata, "^Training")
staffdata_Support <- subset_columns_by_pattern(staffdata, "^Support")

# typos to correct 
## (doesn't work on simulated data for reasons I do not find - and if applied to data_round, it changes all columns to characters...)
pgrdata_Effect <- as.data.frame(lapply(pgrdata_Effect, gsub, pattern = "Neutral(neither detrimental nor beneficial)", 
                                         replacement = "Neutral (neither detrimental nor beneficial)", fixed = TRUE))
staffdata_Effect <- as.data.frame(lapply(staffdata_Effect, gsub, pattern = "Neutral(neither detrimental nor beneficial)", 
                             replacement = "Neutral (neither detrimental nor beneficial)", fixed = TRUE))

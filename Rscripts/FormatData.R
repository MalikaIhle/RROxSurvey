################################################
##    RROx survey: Open research at Oxford    ##
## round 1: PGR - 12 jan 2021 to 1 march 2021 ##
##                 Format data                ##
################################################

rm(list = ls())

# Packages
library(here)
library(tidyverse)
library(RColorBrewer)
library(janitor)
library(ggpubr) # for function ggarange for grid ggplot
library(arsenal) # for function comparedf (to compare dataset created by hand and output of function)

# Functions
source(here::here("Rscripts","Functions-and-Parameters.R"))

# Load data
data_round1 <- read.csv(here("Data/RealData_20210302-0945.csv"), stringsAsFactors=FALSE)
data_round2 <- read.csv(here("Data/SimulatedData_20201214.csv"), stringsAsFactors=FALSE) ## this is simulated data to prep the code for round 2
surveyindex <- read.csv(here("Data/SurveyIndex.csv"), stringsAsFactors=FALSE)
targetnumbers <- read.csv(here("Data/TargetNumbers.csv"), stringsAsFactors=FALSE) # for sample size script

# Format data
data_round1 <- clean_qualtrics_data(data_round1, surveyindex)
data_round2 <- clean_qualtrics_data(data_round2, surveyindex) ## this is simulated data to prep the code for round 2
rm(surveyindex)

# Split data per respondent type
pgrdata <- data_round1[data_round1$StudentStaff == "Student",] # will possibly need to rbind pgrdata collected during round2
staffdata <- rbind(data_round1[data_round1$StudentStaff == "Staff",], data_round2)

# Split data per question
pgrdata_Awareness <- subset_columns_by_pattern(pgrdata, "^Awareness")
pgrdata_Effect <- subset_columns_by_pattern(pgrdata, "^Effect")
pgrdata_Barriers <- subset_columns_by_pattern(pgrdata, "^Barriers")

staffdata_Awareness <- subset_columns_by_pattern(staffdata, "^Awareness")

# Sample size


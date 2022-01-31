# RROxSurvey

round 1: targeted PGR students from 12 jan 2021 to 1 march 2021. the data contains some staff data to mergwe to round 2 data which will target them.

# Data folder
csv files:  
- raw data (RealData)  
- simulated data (to help prepare the coding)   
- index to convert the qualtrics code to column headers   
- census of students and staff total numbers   


# Rscripts

## 1. FormatData.R
Load packages and raw data, and `source("Functions-and-Parameters.R")`
to process the data and create subdatasets for analyses and plotting.


## 2. Functions-and-Parameters.R
Contains the list of items to judges or categories, and functions to clean, summarize and prepare the data for plotting 
(create a skeleton of dataset to merge in real data, summarise items, bind the summaries),
function to create circular plots, and functions to regroup the data previously split by Divisions and to create a stacked bar plot. Function to create composite plot.


## 3. Sample-sizes.R
This script compiles sample sizes of answers per Division, per question, and for staff and students.  
To work on this file on its own, one need to uncomment the first line   
`#source("FormatData.R")`  
This R file is called in LongReport.Rmd which needs this line at the top to be commented out.  


## 4. Question.R

There is one R file per question, which creates plots and tables.  
To work on one file specifically, one need to uncomment the first line   
`#source("FormatPGRdata.R")`  

Each of these R files is called in LongReport.Rmd which needs this line at the top to be commented out.  

The list of file in order of the questions in the survey is:  
- Awareness.R -> Q4 Which of the following research practices are you aware of, and which do you have experience with?  
- Effect.R -> Q5 In your opinion, what would be the overall effect of widespread adoption of the following practices in your field of research?  
- Barriers.R -> Q6 Do you face any barriers in adopting the following practices and, if so, what are they?  
- Downsides.R -> Q7 In your view, are there any downsides to widespread adoption of the following practices in your field of research?  
- OtherBarriersDownsides.R -> free text answers from Q6 and Q7 to categorise.  
- CurrentRecruitment.R <- Q8 To the best of your knowledge, to what extent are the following criteria used for recruitment in your field of research at Oxford?  
- FutureRecruitement.R -> Q9 In your opinion, to what extent should the following criteria be used for recruitment in your field of research at Oxford?  
- Training.R -> Q10 For which of the following topics do you think more guidance is necessary?  
- OtherTraining.R <- free text from Q10  
- Support.R -> Q11 What additional support would you find useful to implement open research practices?  
- OtherSupport.R -> free text from Q11  
- Inventory.R -> Q12 Please provide information to help us identify initiatives at Oxford that relate to open research (e.g. courses, workshops, summer schools, working groups, study groups, meet-ups).  


# Reports
Rmarkdown reports.  

- ShortReport.R -> create combines figures for a landscape draft report made in Gdoc: https://docs.google.com/document/d/1dc3E3oU9evBrBYhIxMUqiEsvxx_bkc_y/edit  
- Report.html -> version that compiled before changing Function-and-Parameters.R for circular plot to take more arguments depending on the question.  
- LongReport.html -> version that compiled after changing Function-and-Parameters.R (commenting out the argument question to the circular plot)  
- For the actually shared executive summary report    https://docs.google.com/document/d/1mxloQSeqGx9fWX_SvNp-t2cISb5OOJWMaPb5ZGy7irs/edit, the figures were created by running the circular plot function 'by hand' (to personalise layout) and saved through the RStudio preview.  

- LongReport.Rmd  
current version of the report.  
Starts with   
`source("FormatData.R")`  
`source("Sample-sizes.R")`  
and then `source("*Question*.R")` in turns.  

It calculates descriptive statistics in R code chunks.  
It calls names of plots and tables created in the Question.R files.  

# Figures
Saving figures for testing command to plot and ajusting.   
The Rmd reports recreate the figures and do not use these still images.   

# list of decisions
- College only staff affiliation replaced by their Division of affinity. if didn't answer that -> data not shown in plots unless all data pooled.
- horizontal bar plot function merges ContEd and GLAM data to Hum data
- if one item not scored in a grid, I remove those NAs for calculation of percentage for each responses for that item. (i.e. like if items were independents)
- for Barriers: select items if at least one answer is not NA to calculate percentages (like if items were independents)
# RROxSurvey

round 1: targeted PGR students from 12 jan 2021 to 1 march 2021.  
round 2: targeted mostly research staff and fellows, but also support staff, academics, and PGR students; from 12 jan 2022 to 1 march 2022.

# renv

the files renv.lock, .Rprofile, and the project library / folder renv, are used to create a reproducible environment. 
One can clone the repository locally, and if opened in RStudio (for easier use), renv will detect what needs to be installed on your local machine.
An 8 min video intro to renv: https://osf.io/kv8h3/ 


# Data folder
csv files:  
- raw data (RealData)  
- simulated data (to help prepare the coding)   
- index to convert the qualtrics code to column headers   
- census of students and staff total numbers   


# Rscripts

## 1. FormatData.R
Load packages and raw data, `source("Functions-and-Parameters.R")` and `source("Sample-sizes.R")`
Process the data and create subdatasets per questions for analyses and plotting.


## 2. Functions-and-Parameters.R
Contains the list of items to judge or categories, and functions
- to clean, summarize and prepare the data for plotting (create a skeleton of dataset to merge in real data, summarise items, bind the summaries),
- to regroup the data previously split by Divisions and to create stacked bar plots
- to create circular plots
- to create composite plots
- to format text in free text boxes


## 3. Sample-sizes.R
This script compiles sample sizes of answers per Division, per question, and for all researcher types.  
To work on this file on its own, one need to uncomment the first line   
`#source("FormatData.R")`  


## 4. Question.R

There is one R file per question, which creates plots and tables.  
To work on one file specifically, one need to uncomment the first line   
`#source("FormatData.R")`  

Each of these R files is called in Rmd reports which needs this line at the top to be commented out.  

The list of file in order of the questions in the survey is:  
- Awareness.R -> Q4 Which of the following research practices are you aware of, and which do you have experience with?  
- Effect.R -> Q5 In your opinion, what would be the overall effect of widespread adoption of the following practices in your field of research?  
- Barriers.R -> Q6 Do you face any barriers in adopting the following practices and, if so, what are they?  
- Downsides.R -> Q7 In your view, are there any downsides to widespread adoption of the following practices in your field of research?  
- OtherBarriersDownsides.R -> free text answers from Q6 and Q7 to categorise.  
- CurrentRecruitment.R <- Q8 To the best of your knowledge, to what extent are the following criteria used for recruitment in your field of research at Oxford?  
- FutureRecruitement.R -> Q9 In your opinion, to what extent should the following criteria be used for recruitment in your field of research at Oxford?  
- RecruitementCombined.R -> creates unified plot from the answers from both previous question (all criteria or single criteria like ORP)
- Training.R -> Q10 For which of the following topics do you think more guidance is necessary?  
- OtherTraining.R <- free text from Q10  
- Support.R -> Q11 What additional support would you find useful to implement open research practices?  
- OtherSupport.R -> free text from Q11  
- Inventory.R -> Q12 Please provide information to help us identify initiatives at Oxford that relate to open research (e.g. courses, workshops, summer schools, working groups, study groups, meet-ups).  


# Reports
Rmarkdown reports, some of which uses a word template 'template-lyngs.docx' to compile.  

Old drafts:
- ShortReport.R -> create combines figures for a landscape draft report made in Gdoc: https://docs.google.com/document/d/1dc3E3oU9evBrBYhIxMUqiEsvxx_bkc_y/edit  
- Report.html -> version that compiled before changing Function-and-Parameters.R for circular plot to take more arguments depending on the question.  
- LongReport.html -> long version at time of round 1 which no longer compiles  
- For the actually shared executive summary report https://docs.google.com/document/d/1mxloQSeqGx9fWX_SvNp-t2cISb5OOJWMaPb5ZGy7irs/edit, the figures were created by running the circular plot function 'by hand' (to personalise layout) and saved through the RStudio preview.  

- OxFOS.Rmd -> the current version of the report, that include both rounds merged, prepared for the Festival of Open Scholarship on 17 March 2022.  
Starts with   
`source("FormatData.R")`  
`source("Sample-sizes.R")`  
and then `source("*Question*.R")` in turns.  

It calculates descriptive statistics in R code chunks.  
It calls names of plots and tables created in the Question.R files.  

# Figures
Saving figures for testing command to plot and adjusting.   
The Rmd reports no longer recreates the figures (unless the commented out ggsave commands in each Question.R are uncommented) and use these still images.   

# list of decisions
- College only staff affiliation replaced by their Division of affinity. if didn't answer that -> data not shown in plots unless all data pooled.
- if one item not scored in a grid, I remove those NAs for calculation of percentage for each responses for that item. (i.e. like if items were independents)
- for Barriers: select items if at least one answer is not NA to calculate percentages (like if items were independents)
- the sample size for a question is the maximum number of unique respondents for that question, even if they did not answer all items.
- the sample size for an item in a grid question is relative to that item.
- for historical reasons, I first split the data per Division (for plotting), then regroup it (for plotting), which is suboptimal but working anyway.







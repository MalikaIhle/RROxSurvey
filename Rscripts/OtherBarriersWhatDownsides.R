# OB = Other Barriers
# WD = What Downsides


# source("Rscripts/FormatData.R")


# Alldata_OB -----
pgrdata_OB <- cbind(prepare_freetext_subdataset(pgrdata, "^OtherBarriers_"), Subdataset = 'pgrdata')
staffdata_OB <- cbind(prepare_freetext_subdataset(staffdata, "^OtherBarriers_"), Subdataset = 'staffdata')
supportstaffdata_OB <- cbind(prepare_freetext_subdataset(supportstaffdata, "^OtherBarriers_"), Subdataset = 'supportstaffdata')
academicdata_OB <- cbind(prepare_freetext_subdataset(academicdata, "^OtherBarriers_"), Subdataset = 'academicdata')
Alldata_OB <- rbind(pgrdata_OB,staffdata_OB,supportstaffdata_OB,academicdata_OB)

## Nb of responses
pgrdata_OB %>% summarise(across (everything(), ~sum(!is.na(.))))
staffdata_OB %>% summarise(across (everything(), ~sum(!is.na(.))))
supportstaffdata_OB %>% summarise(across (everything(), ~sum(!is.na(.))))
academicdata_OB %>% summarise(across (everything(), ~sum(!is.na(.))))
Alldata_OB %>% summarise(across (everything(), ~sum(!is.na(.))))

## check if respondents wrote something like same as previous answer.....
Alldata_OB[unique(c(
  which(str_detect(Alldata_OB$Data, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_OB$Code, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_OB$Materials, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_OB$Preprint, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_OB$Prereg, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_OB$RegRep, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")))),]


## categorise barriers
Alldata_OB$OA_cat <- NA
Alldata_OB$OA_cat2 <- NA
Alldata_OB$Data_cat <- NA
Alldata_OB$Data_cat2 <- NA
Alldata_OB$Code_cat <- NA
Alldata_OB$Materials_cat <- NA
Alldata_OB$Preprint_cat <- NA
Alldata_OB$Preprint_cat2 <- NA
Alldata_OB$Preprint_cat3 <- NA
Alldata_OB$Prereg_cat <- NA
Alldata_OB$RegRep_cat <- NA

### OA
Alldata_OB$OA[!is.na(Alldata_OB$OA)]
Alldata_OB$OA_cat[str_detect(Alldata_OB$OA, "INDUSTRY")] <- 'Resource not owned'
Alldata_OB$OA_cat[str_detect(Alldata_OB$OA, c("EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANC*|PAY|CHARGES|AFFORD|APC"))] <- 'Financial cost'
Alldata_OB$OA_cat2[str_detect(Alldata_OB$OA, "JOURNAL QUALITY|MORE PRESTIGIOUS|OFTEN SEEN AS WORSE|HIGH IMPACT JOURNALS|HIGH QUALITY JOURNALS|HIGH IMPACT FACTOR")] <- 'Lower quality or less prestigious journal'
Alldata_OB$OA_cat2[str_detect(Alldata_OB$OA, "NOT POSSIBLE IN ALL JOURNALS|FITS THE REMIT OF THE JOURNAL|JOURNALS NOT SUPPORTING|JOURNAL BY WHAT WOULD LOOK GOOD")] <- 'Not possible in all journals'
Alldata_OB$OA_cat[str_detect(Alldata_OB$OA, "HTTP://OPENACCESS.OX.AC.UK/|BUREAUCRACY")] <- 'University website unclear, it takes time to find out which journals is OA'
Alldata_OB$OA_cat2[str_detect(Alldata_OB$OA, "ADMIN PEOPLE|CONFUSION OVER POLICY EXPECTATIONS")] <- 'Policy changing rapidly, confusion, University administration unaware of researchers needs'
Alldata_OB$OA_cat2[str_detect(Alldata_OB$OA, "BOOK PUBLISHER FOR SALES LOST")] <- 'Book publishers sales loss'

quote_OB_OA_1 <- data$OtherBarriers_OA[!is.na(data$OtherBarriers_OA) & startsWith(data$OtherBarriers_OA, "Funding for Open Access publishing is insufficient, opaque")==TRUE]
quote_OB_OA_2 <- data$OtherBarriers_OA[!is.na(data$OtherBarriers_OA) & startsWith(data$OtherBarriers_OA, "The cost of open access publishing is an additional barrier")==TRUE]
quote_OB_OA_3 <- data$OtherBarriers_OA[!is.na(data$OtherBarriers_OA) & startsWith(data$OtherBarriers_OA, "Even co-called")==TRUE]
quote_OB_OA_4 <- data$OtherBarriers_OA[!is.na(data$OtherBarriers_OA) & startsWith(data$OtherBarriers_OA, "Open Access, while beneficial for users")==TRUE]


Alldata_OB$OA_cat[!is.na(Alldata_OB$OA) & is.na(Alldata_OB$OA_cat)] <- 'Not categorised'
Alldata_OB$OA[!is.na(Alldata_OB$OA_cat) & Alldata_OB$OA_cat == 'Not categorised']
Alldata_OB$OA_cat[!is.na(Alldata_OB$OA_cat2) & Alldata_OB$OA_cat == 'Not categorised'] <-  Alldata_OB$OA_cat2[!is.na(Alldata_OB$OA_cat2) & Alldata_OB$OA_cat == 'Not categorised']
Alldata_OB$OA_cat2[!is.na(Alldata_OB$OA_cat2) & Alldata_OB$OA_cat == Alldata_OB$OA_cat2] <- NA
Alldata_OB$OA[!is.na(Alldata_OB$OA_cat) & Alldata_OB$OA_cat == 'Not categorised']
# View(Alldata_OB[,c('OA','OA_cat','OA_cat2')])

table(c(Alldata_OB$OA_cat, Alldata_OB$OA_cat2))


## Data
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "MANAGING DATA|TB OF DATA")] <- 'Difficult resource management and lack of metadata standards'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|ETHICAL REGULATIONS|MUST NOT BE SHARED|CONFIDENTIALITY|IDENTIFIABILITY|CONSENT|PATIENTS INFORMATION")] <- 'Ethical concerns'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "AUTHORITY|INDUSTRY|COMMERCIAL*|PARTNERS|POLITICS")] <- 'Resource not owned'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "OEUVERS")] <- 'Resource not always digital'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "FUNDING FOR DATA CLEANING|ALLOCATION OF RELEVANT RESEARCHERS|EFFORT OF COLLECTING AND PROCESSING THE DATA IS IGNORED")] <- 'Support/funding for preparing resource to share'
Alldata_OB$Data_cat2[str_detect(Alldata_OB$Data, "LACK OF NORMS ON AN APPROPRIATE EMBARGO PERIOD|FAILURE TO BENEFIT FROM OUR OWN HARD WORK|DATA MAY BE USED FOR OTHER PUBLICATIONS")] <- 'Original effort/cost of resource collection is ignored'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "ENABLE STORAGE OF DATA")] <- 'Funding to store resource'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "HOW USEFUL")] <- 'Not useful, no reusability'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "USER-UNFRIENDLY")] <- 'Repositories not user-friendly'

quote_OB_Data_1 <- data$OtherBarriers_Data[!is.na(data$OtherBarriers_Data) & startsWith(data$OtherBarriers_Data, "It is not trivial to reconcile")==TRUE]
quote_OB_Data_2 <- data$OtherBarriers_Data[!is.na(data$OtherBarriers_Data) & startsWith(data$OtherBarriers_Data, "Lack of norms on an appropriate embargo period")==TRUE]
quote_OB_Data_3 <- data$OtherBarriers_Data[!is.na(data$OtherBarriers_Data) & startsWith(data$OtherBarriers_Data, "Ethnographic fieldnotes cannot")==TRUE]


Alldata_OB$Data_cat[!is.na(Alldata_OB$Data) & is.na(Alldata_OB$Data_cat)] <- 'Not categorised'
Alldata_OB$Data[!is.na(Alldata_OB$Data_cat) & Alldata_OB$Data_cat == 'Not categorised']
Alldata_OB$Data_cat[!is.na(Alldata_OB$Data_cat2) & Alldata_OB$Data_cat == 'Not categorised'] <-  Alldata_OB$Data_cat2[!is.na(Alldata_OB$Data_cat2) & Alldata_OB$Data_cat == 'Not categorised']
Alldata_OB$Data_cat2[!is.na(Alldata_OB$Data_cat2) & Alldata_OB$Data_cat == Alldata_OB$Data_cat2] <- NA
Alldata_OB$Data[!is.na(Alldata_OB$Data_cat) & Alldata_OB$Data_cat == 'Not categorised']
# View(Alldata_OB[,c('Data','Data_cat','Data_cat2')])

table(c(Alldata_OB$Data_cat, Alldata_OB$Data_cat2))


## Code
Alldata_OB$Code_cat[str_detect(Alldata_OB$Code, "PRIOR TO PUBLICATION|BEFORE THE PUBLICATION")] <- 'Fear of scooping'
Alldata_OB$Code_cat[str_detect(Alldata_OB$Code, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|ETHICAL REGULATIONS|MUST NOT BE SHARED|CONFIDENTIALITY|IDENTIFIABILITY|CONSENT")] <- 'Ethical concerns'
Alldata_OB$Code_cat[str_detect(Alldata_OB$Code, "TIME|LOT OF WORK")] <- 'Time investment'
Alldata_OB$Code_cat[str_detect(Alldata_OB$Code, "NO BENEFIT|HOW USEFUL")] <- 'Not useful'
Alldata_OB$Code_cat[str_detect(Alldata_OB$Code, "USER-UNFRIENDLY")] <- 'Repositories not user-friendly'

quote_OB_Code_1 <- data$OtherBarriers_Code[!is.na(data$OtherBarriers_Code) & startsWith(data$OtherBarriers_Code, "The main barrier is time needed to learn")==TRUE]


Alldata_OB$Code_cat[!is.na(Alldata_OB$Code) & is.na(Alldata_OB$Code_cat)] <- 'Not categorised'
Alldata_OB$Code[!is.na(Alldata_OB$Code) & Alldata_OB$Code_cat == 'Not categorised']
#View(Alldata_OB[,c('Code', 'Code_cat')])

table(Alldata_OB$Code_cat)

## Materials
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|ETHICAL REGULATIONS|MUST NOT BE SHARED|CONFIDENTIALITY|IDENTIFIABILITY|CONSENT")] <- 'Ethical concerns'
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "MANAGING DATA|HARD TO FIND TIME TO CATALOGUE MATERIAL FOR DISCOVERABLE STORAGE")] <- 'Difficult resource management and lack of metadata standards'
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "LIBRARIES")] <- 'Resource not always digital'
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "TIME IT TAKES FOR IT TO ARRIVE")] <- 'Original effort/cost of resource collection is ignored'
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "USER-UNFRIENDLY")] <- 'Repositories not user-friendly'
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "NO BENEFIT|HOW USEFUL")] <- 'Not useful'
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "STORING CRYO")] <- 'Funding to store resource'
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "AUTHORITY|INDUSTRY|COMMERCIAL*|PARTNERS|POLITICS")] <- 'Resource not owned'

Alldata_OB$Materials_cat[!is.na(Alldata_OB$Materials) & is.na(Alldata_OB$Materials_cat)] <- 'Not categorised'
Alldata_OB$Materials[!is.na(Alldata_OB$Materials) & Alldata_OB$Materials_cat == 'Not categorised']
# View(Alldata_OB[,c('Materials','Materials_cat')])

quote_OB_Materials_1 <- data$OtherBarriers_Materials[!is.na(data$OtherBarriers_Materials) & startsWith(data$OtherBarriers_Materials, "I find it hard to find time")==TRUE]

table(Alldata_OB$Materials_cat)


## Preprint
Alldata_OB$Preprint_cat[str_detect(Alldata_OB$Preprint, "COPYRIGHT|DEPENDS ON FUNDING BODY|NOT SURE WHEN IT IS A PROBLEM FOR A JOURNAL|JOURNALS MAY HAVE RESTRICTIONS|JOURNALS WON'T ACCEPT PAPERS|JOURNALS NOT ACCEPTING")] <- 'Intellectual property concerns' 
Alldata_OB$Preprint_cat2[str_detect(Alldata_OB$Preprint, "PEER REVIEW|HAVE NOT BEEN CHECKED FOR ACCURACY")] <- 'Lack of peer review'
Alldata_OB$Preprint_cat3[str_detect(Alldata_OB$Preprint, "PRIOR TO PUBLICATION|SCOOPED|SCOOPED")] <- 'Fear of scooping'
Alldata_OB$Preprint_cat[str_detect(Alldata_OB$Preprint, "ARXIV")] <- 'Not recognised as meeting OA requirements (e.g. for REF)'
Alldata_OB$Preprint_cat[str_detect(Alldata_OB$Preprint, "USER-UNFRIENDLY")] <- 'Repositories not user-friendly'
Alldata_OB$Preprint_cat[str_detect(Alldata_OB$Preprint, "ORA")] <- 'Duplication when several authors add to ORA'

Alldata_OB$Preprint_cat[!is.na(Alldata_OB$Preprint) & is.na(Alldata_OB$Preprint_cat)] <- 'Not categorised'
Alldata_OB$Preprint[!is.na(Alldata_OB$Preprint) & Alldata_OB$Preprint_cat == 'Not categorised']
Alldata_OB$Preprint_cat[!is.na(Alldata_OB$Preprint_cat2) & Alldata_OB$Preprint_cat == 'Not categorised'] <- Alldata_OB$Preprint_cat2[!is.na(Alldata_OB$Preprint_cat2) & Alldata_OB$Preprint_cat == 'Not categorised']
Alldata_OB$Preprint_cat2[!is.na(Alldata_OB$Preprint_cat2) & Alldata_OB$Preprint_cat == Alldata_OB$Preprint_cat2] <- NA
Alldata_OB$Preprint[!is.na(Alldata_OB$Preprint) & Alldata_OB$Preprint_cat == 'Not categorised']
Alldata_OB$Preprint_cat[!is.na(Alldata_OB$Preprint_cat3) & Alldata_OB$Preprint_cat == 'Not categorised'] <- Alldata_OB$Preprint_cat3[!is.na(Alldata_OB$Preprint_cat3) & Alldata_OB$Preprint_cat == 'Not categorised']
Alldata_OB$Preprint_cat3[!is.na(Alldata_OB$Preprint_cat3) & Alldata_OB$Preprint_cat == Alldata_OB$Preprint_cat3] <- NA
Alldata_OB$Preprint[!is.na(Alldata_OB$Preprint) & Alldata_OB$Preprint_cat == 'Not categorised']
# View(Alldata_OB[,c('Preprint','Preprint_cat','Preprint_cat2', 'Preprint_cat3')])

quote_OB_Preprint_1 <- data$OtherBarriers_Preprint[!is.na(data$OtherBarriers_Preprint) & startsWith(data$OtherBarriers_Preprint, "Not sure when it is a problem for a journal")==TRUE]


table(Alldata_OB$Preprint_cat)


## Preregistration
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "PRIOR TO PUBLICATION|UNSCRUPULOUS APPROPRIATION OF IDEAS|AN UNKNOWN REGISTER")] <- 'Fear of scooping'
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "PILOT")] <- 'Lack of funding for pilot studies'
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "SHOULD NOT BE APPLIED UNIVERSALLY|DISCIPLINE|NOT ALL WORK IS WELL SUITED|HARD|UNREALISTIC|MADNESS")] <- 'Not applicable to all disciplines, or difficult'
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "FUNDING")] <- 'Depend on funding body'

quote_OB_Prereg_1 <- data$OtherBarriers_Prereg[!is.na(data$OtherBarriers_Prereg) & startsWith(data$OtherBarriers_Prereg, "In my field we have to apply for beam time")==TRUE]
quote_OB_Prereg_2 <- data$OtherBarriers_Prereg[!is.na(data$OtherBarriers_Prereg) & startsWith(data$OtherBarriers_Prereg, "funding to run a pilot study prior")==TRUE]

Alldata_OB$Prereg_cat[!is.na(Alldata_OB$Prereg) & is.na(Alldata_OB$Prereg_cat)] <- 'Not categorised'
Alldata_OB$Prereg[!is.na(Alldata_OB$Prereg) & Alldata_OB$Prereg_cat == 'Not categorised']
# View(Alldata_OB[,c('Prereg','Prereg_cat')])

table(Alldata_OB$Prereg_cat)


## Registered Report
Alldata_OB$RegRep_cat[str_detect(Alldata_OB$RegRep, "PRIOR TO PUBLICATION|UNSCRUPULOUS APPROPRIATION OF IDEAS|AN UNKNOWN REGISTER")] <- 'Fear of scooping'
Alldata_OB$RegRep_cat[str_detect(Alldata_OB$RegRep, "PILOT")] <- 'Lack of funding for pilot studies'
Alldata_OB$RegRep_cat[str_detect(Alldata_OB$RegRep, "SHOULD NOT BE APPLIED UNIVERSALLY|DISCIPLINE|NOT ALL WORK IS WELL SUITED|HARD|UNREALISTIC|MADNESS|COMPLEXITY")] <- 'Not applicable to all disciplines, or difficult'
Alldata_OB$RegRep_cat[str_detect(Alldata_OB$RegRep, "FUNDING")] <- 'Depend on funding body'
Alldata_OB$RegRep_cat[str_detect(Alldata_OB$RegRep, "MESSY")] <- 'Impedes flexibility in protocols'
Alldata_OB$RegRep_cat[str_detect(Alldata_OB$RegRep, "TIMESCALE|TIME-FRAME|NOT ALWAYS TIME TO ALLOW FOR REVIEW")] <- 'Timescale difficult to integrate in research projects'
Alldata_OB$RegRep_cat[str_detect(Alldata_OB$RegRep, "PEER REVIEWERS|ADDITIONAL BURDEN ON OUR COLLEAGUE TO REVIEW")] <- 'Too much work for peer-reviewers'
Alldata_OB$RegRep_cat[str_detect(Alldata_OB$RegRep, "JOURNALS IN SOME FIELDS")] <- 'Lack of suitable journals'

Alldata_OB$RegRep_cat[!is.na(Alldata_OB$RegRep) & is.na(Alldata_OB$RegRep_cat)] <- 'Not categorised'
Alldata_OB$RegRep[!is.na(Alldata_OB$RegRep) & Alldata_OB$RegRep_cat == 'Not categorised']
# View(Alldata_OB[,c('RegRep','RegRep_cat')])

quote_OB_RegRep_1 <- data$OtherBarriers_RegRep[!is.na(data$OtherBarriers_RegRep) & startsWith(data$OtherBarriers_RegRep, "In my field (experimental molecular biology)")==TRUE]
quote_OB_RegRep_2 <- data$OtherBarriers_RegRep[!is.na(data$OtherBarriers_RegRep) & startsWith(data$OtherBarriers_RegRep, "Imagine having more work")==TRUE]

table(Alldata_OB$RegRep_cat)





# Alldata_WD -----
pgrdata_WD <- cbind(prepare_freetext_subdataset(pgrdata, "^WhatDownsides_"), Subdataset = 'pgrdata')
staffdata_WD <- cbind(prepare_freetext_subdataset(staffdata, "^WhatDownsides_"), Subdataset = 'staffdata')
supportstaffdata_WD <- cbind(prepare_freetext_subdataset(supportstaffdata, "^WhatDownsides_"), Subdataset = 'supportstaffdata')
academicdata_WD <- cbind(prepare_freetext_subdataset(academicdata, "^WhatDownsides_"), Subdataset = 'academicdata')
Alldata_WD <- rbind(pgrdata_WD,staffdata_WD,supportstaffdata_WD,academicdata_WD)

## Nb of responses
pgrdata_WD %>% summarise(across (everything(), ~sum(!is.na(.))))
staffdata_WD %>% summarise(across (everything(), ~sum(!is.na(.))))
supportstaffdata_WD %>% summarise(across (everything(), ~sum(!is.na(.))))
academicdata_WD %>% summarise(across (everything(), ~sum(!is.na(.))))
Alldata_WD %>% summarise(across (everything(), ~sum(!is.na(.))))

## check if respondents wrote something like 'same as previous answer'..... this is a dangerous move to overwrite answers!! Absolutely need checking with new data!!
##### didn't overwrite all those occurences, as some refered to previous question on barriers
Cells_to_fillup_manually <- Alldata_WD[unique(c(
  which(str_detect(Alldata_WD$Data, "SEE ABOVE|SEE PREVIOUS|AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Code, "SEE ABOVE|SEE PREVIOUS|AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Materials, "SEE ABOVE|SEE PREVIOUS|AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Preprint, "SEE ABOVE|SEE PREVIOUS|AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Prereg, "SEE ABOVE|SEE PREVIOUS|AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$RegRep, "SEE ABOVE|SEE PREVIOUS|AS ABOVE|AS FOR|AS IN|SAME AS|\\^")))),]

### line 6
Alldata_WD$Data[!is.na(Alldata_WD$Data) & Alldata_WD$Data == 'SAME AS OPEN ACCESS PUBLICATION'] <- Alldata_WD$OA[!is.na(Alldata_WD$Data) & Alldata_WD$Data == 'SAME AS OPEN ACCESS PUBLICATION']
Alldata_WD$Materials[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials == 'SAME AS OPEN ACCESS PUBLICATION'] <- Alldata_WD$OA[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials == 'SAME AS OPEN ACCESS PUBLICATION']

### line 9
Alldata_WD$Data[!is.na(Alldata_WD$Data) & Alldata_WD$Data == '^'] <- Alldata_WD$OA[!is.na(Alldata_WD$Data) & Alldata_WD$Data == '^']
Alldata_WD$Code[!is.na(Alldata_WD$Code) & Alldata_WD$Code == '^'] <- Alldata_WD$OA[!is.na(Alldata_WD$Code) & Alldata_WD$Code == '^']
Alldata_WD$Materials[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials == '^'] <- Alldata_WD$OA[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials == '^']

### line 52
Alldata_WD$Materials[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials == 'AS FOR DATA. '] <- Alldata_WD$Data[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials == 'AS FOR DATA. ']

### line 53
Alldata_WD$RegRep[!is.na(Alldata_WD$RegRep) & Alldata_WD$RegRep == 'SAME AS FOR PREREGISTRATION.'] <- Alldata_WD$Prereg[!is.na(Alldata_WD$RegRep) & Alldata_WD$RegRep == 'SAME AS FOR PREREGISTRATION.']

### line 170
Alldata_WD$Materials[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials == 'SAME AS THOSE FOR DATA'] <- Alldata_WD$Data[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials == 'SAME AS THOSE FOR DATA']

### line 251
Alldata_WD$Materials[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials == 'SAME AS ABOVE'] <- Alldata_WD$Data[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials == 'SAME AS ABOVE']

rm(Cells_to_fillup_manually)


## categorise downsides
Alldata_WD$OA_cat <- NA
Alldata_WD$Data_cat <- NA
Alldata_WD$Data_cat2 <- NA
Alldata_WD$Code_cat <- NA
Alldata_WD$Code_cat2 <- NA
Alldata_WD$Materials_cat <- NA
Alldata_WD$Preprint_cat <- NA
Alldata_WD$Preprint_cat2 <- NA
Alldata_WD$Prereg_cat <- NA
Alldata_WD$Prereg_cat2 <- NA
Alldata_WD$RegRep_cat <- NA
Alldata_WD$RegRep_cat2 <- NA
Alldata_WD$RegRep_cat3 <- NA

### OA
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "HARM*|INAPPROPRIATE")] <- 'No control over validity of reuse, misrepresentation, misuse'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANC*|PAY|CHARGES|EXPENSE")] <- 'Financial cost' # need to be first to ahve a couple of overwrites below
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "JOURNAL INCOME|PRODUCTION|INCOME FROM JOURNAL SUBSCRIPTIONS|DAMAGING TO PUBLISHERS|DISINCENTIVE FOR PUBLISHERS|LEARNED SOCIETIES|PUBLISHERS KEEP TELLING US|DISINCENTIVISES PURCHASES OF PRINT|THREATENS THE FINANCING OF PUBLISHERS|OUT OF BUSINESS|DRIVING OUT PUBLISHERS OF MONOGRAPHS|ACADEMIC BOOK PUBLISHING")] <- "Loss of scholarly societies, authors, or publishers' income (e.g. for books)" # need to find other means of journal production"
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "REDUCTION IN OVERALL QUALITY|NOT ALWAYS OF PARTICULARLY HIGH QUALITY|QUALITY OF PUBLICATION|RIGOR*|POOR-QUALITY|LACK OF QUALITY CONTROL|REDUCE THE QUALITY OF PEER REVIEW IN JOURNALS|LOWERING THE QUALITY|MAY REDUCE EDITORIAL INPUT")] <- 'Lowers quality' # reduce quality of peer review if journal paid
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "OPTIONS|LIMITS THE JOURNALS|IMPORTANT JOURNALS|REPUTABLE PRESSES|LESS PRESTIGIOUS|INABILITY TO WORK WITH TOP PUBLISHERS")] <- 'Fewer (prestigious) journal options'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "OPINION|HARMFUL APPLICATIONS|HARASSMENT")] <- 'Ethical, safety, or security concerns'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "ADMINISTRATIVE|SLOW DOWN PUBLICATIONS|TOO MUCH OF A BURDEN")] <- 'Too much administrative time'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "PREDATORY|USE THIS TREND AS AN OPPORTUNITY TO INCREASE THEIR REVENUE|A WAY TO GET MORE MONEY|PUBLISHERS HAVE CARTE BLANCHE|PUBLISH TOO MANY PAPERS")] <- 'Creates predatory behaviours'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "FUNDING AVAILABILITY COULD BECOME MORE IMPORTANT THAN QUALITY|EXCLUSI*|EXCLUD*|INEQUALIT*|POORER|PEOPLE WITH LOTS OF MONEY|LOW INCOME REGIONS|WEALTHY|WHEALTIER|MORE AFFLUENT INSTITUTIONS GET PREFERENTIAL|COMPETITIVE ADVANTAGE|DEVELOPING COUNTRIES OUT|DETRIMENTAL FOR DIVERSITY|RESOURCE-LIMITED RESEARCHERS|MANY RESEARCHERS CANNOT AFFORD|MINORITISED GROUPS|OTHERS WITHOUT DEDICATED FUNDING|RESEARCHERS WITHOUT FUNDING|ONLY THOSE WITH FUNDING|HARDER FOR THOSE|MIDDLE-INCOME|TO SOME GROUPS|ONLY FAVOURS|POORLY|DISCRIMINATES|THIS IMPACTS MORE|HISTORICALLY DISADVANTAGED AREAS|DETRIMENTAL TO SUCH FIELDS")] <- 'Creates inequalities between researchers/fields/institutions with/without access to funding for APC'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "PREFER HAVING THE HARD COPY OF A BOOK")] <- 'OA requires electronical versions, prefer hard copy of books'

quote_WD_OA_1 <- data$WhatDownsides_OA[!is.na(data$WhatDownsides_OA) & startsWith(data$WhatDownsides_OA, "Journals have passed truly excessive costs")==TRUE]
quote_WD_OA_2 <- data$WhatDownsides_OA[!is.na(data$WhatDownsides_OA) & startsWith(data$WhatDownsides_OA, "Lowers barrier to article submission")==TRUE]
quote_WD_OA_3 <- data$WhatDownsides_OA[!is.na(data$WhatDownsides_OA) & startsWith(data$WhatDownsides_OA, "Gold open access articles are read and cited more")==TRUE]
quote_WD_OA_4 <- data$WhatDownsides_OA[!is.na(data$WhatDownsides_OA) & startsWith(data$WhatDownsides_OA, "The move towards open science publications")==TRUE]
quote_WD_OA_5 <- data$WhatDownsides_OA[!is.na(data$WhatDownsides_OA) & startsWith(data$WhatDownsides_OA, "If the focus is on gold OA")==TRUE]
quote_WD_OA_6 <- data$WhatDownsides_OA[!is.na(data$WhatDownsides_OA) & startsWith(data$WhatDownsides_OA, "I work in a field that does not attract a lot")==TRUE]
quote_WD_OA_7 <- data$WhatDownsides_OA[!is.na(data$WhatDownsides_OA) & startsWith(data$WhatDownsides_OA, "Open Access publication is a laudable goal")==TRUE]
quote_OB_OA_4 <- data$OtherBarriers_OA[!is.na(data$OtherBarriers_OA) & startsWith(data$OtherBarriers_OA, "Open Access, while beneficial for users")==TRUE]
quote_WD_OA_8 <- data$WhatDownsides_OA[!is.na(data$WhatDownsides_OA) & startsWith(data$WhatDownsides_OA, "Author-pays funding model")==TRUE]


Alldata_WD$OA_cat[!is.na(Alldata_WD$OA) & is.na(Alldata_WD$OA_cat)] <- 'Not categorised'
Alldata_WD$OA[!is.na(Alldata_WD$OA) & Alldata_WD$OA_cat == 'Not categorised']
# View(Alldata_WD[,c('OA','OA_cat')])

table(Alldata_WD$OA_cat)


## Data --- not very well done, many answers could have fall within several of the categories created here. 
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN|NOT WITHIN THE SCOPE")] <- 'Time investment' # more work, not valued for career, significant burden for qualitative researchers
Alldata_WD$Data_cat2[str_detect(Alldata_WD$Data, "CONTINUITY")] <- 'Challenges around continuity of ownership (e.g. for longitudinal dataset)'
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "ANONYM*|SENSITIVE DATA|PRIVA*|PARTICIPANT DATA|DATA PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|TRICKY|DPA|LOOTING OF ARCHAEOLOGICAL SITES|SENSITIVE INFORMATION|INFORMED CONSENT|BETRAYING THE TRUST|SENSITIVE NATURE|SENSITIVE MATERIAL|MUST NOT BE SHARED|PATIENTS INFORMATION")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "LICENCES|LICENSES|FORMATS|PROGRAMMES FOR ACQUIRING DATA")] <- 'Proprietary file format'
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "HARMFUL|MALICIOUS|MISUSE|MALIGN|FALSE REPORTING OF RESULTS|MIS-INTERPRETED|MISINTERPRETATION|INAPPROPRIATE ANALYSIS|OUT OF CONTEXT")] <- 'No control over validity of reuse, misrepresentation, misuse'
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "PIPPED|STEALING|SCOOPED|PRE-EMPTIVELY USED|ACADEMIC COPYING")] <- 'Fear of scooping'
Alldata_WD$Data_cat2[str_detect(Alldata_WD$Data, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|CREDIT|COPYRIGHT*|PROTECTION OF IP|USE OF DATA FOR INDUSTRIAL PURPOSES WITHOUT THE DIRECT CONSENT|AUTHORSHIP|LACK OF APPROVAL OR PAYMENT")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation'
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "FIRST TO PUBLISH")] <- 'Lowers quality' # by increasing \'first to publish\' pressure' 
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "NO NORM FOR CITATION|FORMALLY CITABLE|APPROPRIATELY CITED|LACK OF ACKNOWLEDGEMENT")] <- 'No norm for citation'
Alldata_WD$Data_cat2[str_detect(Alldata_WD$Data, "LACK OF NORMS ON AN APPROPRIATE EMBARGO PERIOD|FAILURE TO BENEFIT FROM OUR OWN HARD WORK|DATA MAY BE USED FOR OTHER PUBLICATIONS|TRAGEDY OF THE COMMONS|AS WOULD EMBARGOS|WILL PROFIT FROM THE ALREADY AVAILABLE DATA|BEFORE WE HAVE A CHANCE TO OURSELVES|COMPETITIVE ADVANTAGE|EFFORT OF COLLECTING AND PROCESSING THE DATA IS IGNORED|APPROPRIATE ACKNOWLEDGEMENT OF PEOPLE WHO COLLECTED THE DATA|PREFERENTIAL ACCESS TO DATA|USE BY INDUSTRY WITHOUT SHARING BACK|REMOVE THE INCENTIVES TO RUN A PRIMARY STUDY|WHEN DATA IS DONE WITH|THEY FEEL THEY LOSE CONTROL")] <- 'Original effort/cost of resource collection is ignored, tragedy of the common'
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "HOW USEFUL|INCOMPLETE AND HENCE LESS VALUE|OVER SATURATION")] <- 'Not useful, no reusability or over-saturation'
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "STANDARD WITHIN THE FIELD|FORMAT THAT IS USEFUL")] <- 'Lack of metadata standards'

Alldata_WD$Data_cat[!is.na(Alldata_WD$Data) & is.na(Alldata_WD$Data_cat)] <- 'Not categorised'
Alldata_WD$Data[!is.na(Alldata_WD$Data) & Alldata_WD$Data_cat == 'Not categorised']
Alldata_WD$Data_cat[!is.na(Alldata_WD$Data_cat2) & Alldata_WD$Data_cat == 'Not categorised'] <-  Alldata_WD$Data_cat2[!is.na(Alldata_WD$Data_cat2) & Alldata_WD$Data_cat == 'Not categorised']
Alldata_WD$Data_cat2[!is.na(Alldata_WD$Data_cat2) & Alldata_WD$Data_cat == Alldata_WD$Data_cat2] <- NA
Alldata_WD$Data[!is.na(Alldata_WD$Data) & Alldata_WD$Data_cat == 'Not categorised']

quote_WD_Data_1 <- data$WhatDownsides_Data[!is.na(data$WhatDownsides_Data) & startsWith(data$WhatDownsides_Data, "The problem with widespread adoption")==TRUE]
quote_WD_Data_2 <- data$WhatDownsides_Data[!is.na(data$WhatDownsides_Data) & startsWith(data$WhatDownsides_Data, "Data collection and data cleaning")==TRUE]
quote_WD_Data_3 <- data$WhatDownsides_Data[!is.na(data$WhatDownsides_Data) & startsWith(data$WhatDownsides_Data, "We are solely grant funded")==TRUE]
quote_WD_Data_4 <- data$WhatDownsides_Data[!is.na(data$WhatDownsides_Data) & startsWith(data$WhatDownsides_Data, "As a qualitative researcher, data sharing of fieldnotes")==TRUE]
quote_WD_Data_5 <- data$WhatDownsides_Data[!is.na(data$WhatDownsides_Data) & startsWith(data$WhatDownsides_Data, "Data that were once considered anonymous")==TRUE]
quote_WD_Data_6 <- data$WhatDownsides_Data[!is.na(data$WhatDownsides_Data) & startsWith(data$WhatDownsides_Data, "Data without curation and standards")==TRUE]
quote_WD_Data_7 <- data$WhatDownsides_Data[!is.na(data$WhatDownsides_Data) & startsWith(data$WhatDownsides_Data, "Data collectors are not appropriately")==TRUE]
quote_OB_Data_3 <- data$OtherBarriers_Data[!is.na(data$OtherBarriers_Data) & startsWith(data$OtherBarriers_Data, "Ethnographic fieldnotes cannot")==TRUE]


table(c(Alldata_WD$Data_cat, Alldata_WD$Data_cat2))
# View(Alldata_WD[!is.na(Alldata_WD$Data),c('Data', 'Data_cat','Data_cat2')])


## Code
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "POOR QUALITY|REVIEWED AND TESTED|LAWED CODE COULD GET REUSED|QUALITY OF THE CODE MUST BE MAINTAINED|BETTER PEER REVIEW")] <- 'Lowers quality if unreviewed' # by propagating unreviewed and untested code
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "LONG TIME|MORE WORK|WORKLOAD|OVERHEAD|BURDEN|NOT WITHIN THE SCOPE|SLOW DOWN|TIME CONSUMING")] <- 'Time investment'
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|HARMFUL|MUST NOT BE SHARED|ATTACKING")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "FIRST TO PUBLISH")] <- 'Lowers quality' # by increasing \'first to publish\' pressure
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "MALICIOUS|MISUSE|INAPPRORPIATE|NOT UNDERSTAND|NUANCE|OUT OF CONTEXT|MIS-INTERPRETED|COULD BE MODIFIED")] <- 'No control over validity of reuse, misrepresentation, misuse'
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "REDUCE THE NUMBER OF APPROACHES USED")] <- 'Reduce diversity of approaches'
Alldata_WD$Code_cat2[str_detect(Alldata_WD$Code, "FIND PEOPLE TO REVIEW THE CODE")] <- 'No time or expertise to review code'
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED|COMMERCIAL SENSITIVITY|PROTECTION OF IP|INTELLECTUAL PROPERTY|IP LIMITATIONS|AUTHORSHIP|DIRECT CONSENT OF AUTHORS|INTELLECTUAL PROPERTY|LICENSING")] <- 'Intellectual property concerns' # (including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "WHEN CODE IS DONE WITH|WITHOUT SHARING BACK|COMPETITIVE ADVANTAGE|REAPED MORE REWARDS|COMPETITIVE ADVANTAGE|TRAGEDY OF THE COMMONS")] <- 'Original effort/cost of resource collection is ignored, tragedy of the common'
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "USEABLE BY OTHERS|LESS USE")] <- 'Hard to make it reusable'
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "PIPPED|STEALING|SCOOPED|PRE-EMPTIVELY USED|ACADEMIC COPYING")] <- 'Fear of scooping'
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "NO NORM FOR CITATION|FORMALLY CITABLE|APPROPRIATELY CITED|LACK OF ACKNOWLEDGEMENT")] <- 'No norm for citation'


quote_WD_Code_1 <- data$WhatDownsides_Code[!is.na(data$WhatDownsides_Code) & startsWith(data$WhatDownsides_Code, "Need to develop system for ensuring")==TRUE]
quote_WD_Code_2 <- data$WhatDownsides_Code[!is.na(data$WhatDownsides_Code) & startsWith(data$WhatDownsides_Code, "Flawed code could get reused")==TRUE]
quote_WD_Code_3 <- data$WhatDownsides_Code[!is.na(data$WhatDownsides_Code) & startsWith(data$WhatDownsides_Code, "Considerable extra work in making")==TRUE]
quote_WD_Code_4 <- data$WhatDownsides_Code[!is.na(data$WhatDownsides_Code) & startsWith(data$WhatDownsides_Code, "It is my impression that the coding community")==TRUE]


Alldata_WD$Code_cat[!is.na(Alldata_WD$Code) & is.na(Alldata_WD$Code_cat)] <- 'Not categorised'
Alldata_WD$Code[!is.na(Alldata_WD$Code) & Alldata_WD$Code_cat == 'Not categorised']
Alldata_WD$Code_cat[!is.na(Alldata_WD$Code_cat2) & Alldata_WD$Code_cat == 'Not categorised'] <- Alldata_WD$Code_cat2[!is.na(Alldata_WD$Code_cat2) & Alldata_WD$Code_cat == 'Not categorised']
Alldata_WD$Code_cat2[!is.na(Alldata_WD$Code_cat2) & Alldata_WD$Code_cat == Alldata_WD$Code_cat2] <- NA
Alldata_WD$Code[!is.na(Alldata_WD$Code) & Alldata_WD$Code_cat == 'Not categorised']

table(c(Alldata_WD$Code_cat, Alldata_WD$Code_cat2))
# View(Alldata_WD[!is.na(Alldata_WD$Code), c('Code', 'Code_cat', 'Code_cat2')])

## Materials
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|RADIOACTIVE|MUST NOT BE SHARED|DANGEROUS")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED|AUTHORSHIP")] <- 'Intellectual property concerns' # (including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "FIRST TO PUBLISH")] <- 'Lowers quality'# by increasing \'first to publish\' pressure
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "PIPPED|STEALING|SCOOPED|PUBLISH PAPERS FAST")] <- 'Fear of scooping' # leading to loss of career prospect
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "HARMFUL|MALICIOUS|MISUSE|INAPPRORPIATE|NOT UNDERSTAND|NUANCE|MISINTERPRETATION|OUT OF CONTEXT")] <- 'No control over validity of reuse, misrepresentation, misuse'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "RETURNED|LICENCES|LICENSES")] <- 'Resource not owned' # (e.g. archelogical artifacts)
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN|TIME-CONSUMING|SLOW DOWN")] <- 'Time investment' # more work, not valued for career, significant burden for qualitative researchers
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "PLATFORMS")] <- 'Unusable if not from open source platforms' # 
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "NATIONAL POLICIES|INSTITUTIONAL REQUIREMENTS")] <- 'Law against sharing specific material easily'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "FRAGILE")] <- 'Impractical'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "WHEN CODE IS DONE WITH|WITHOUT SHARING BACK|COMPETITIVE ADVANTAGE|REAPED MORE REWARDS|COMPETITIVE ADVANTAGE|TRAGEDY OF THE COMMONS")] <- 'Original effort/cost of resource collection is ignored, tragedy of the common'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "LARGER COSTS FOR MAINTAINING|TOO EXTENSIVE|EXPENSIVE")] <- 'Huge cost for maintenance'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "ONLY POSSIBLE FOR LARGE|EQUAL ACCESS TO ALL")] <- 'Difficult to give equal access'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "ANONYM*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|HARMFUL|MUST NOT BE SHARED|ATTACKING|BETRAYING THE TRUST")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "STANDARD WITHIN THE FIELD|FORMAT THAT IS USEFUL|ALONG WITH STANDARDS")] <- 'Lack of metadata standards'

quote_WD_Materials_1 <- data$WhatDownsides_Materials[!is.na(data$WhatDownsides_Materials) & startsWith(data$WhatDownsides_Materials, "Sharing material is different")==TRUE]
quote_WD_Materials_2 <- data$WhatDownsides_Materials[!is.na(data$WhatDownsides_Materials) & startsWith(data$WhatDownsides_Materials, "Biological / scientific samples")==TRUE]
quote_WD_Materials_3 <- data$WhatDownsides_Materials[!is.na(data$WhatDownsides_Materials) & startsWith(data$WhatDownsides_Materials, "Providing standardised")==TRUE]
quote_WD_Materials_4 <- data$WhatDownsides_Materials[!is.na(data$WhatDownsides_Materials) & startsWith(data$WhatDownsides_Materials, "Again, the ownership")==TRUE]


Alldata_WD$Materials_cat[!is.na(Alldata_WD$Materials) & is.na(Alldata_WD$Materials_cat)] <- 'Not categorised'
Alldata_WD$Materials[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials_cat == 'Not categorised']
# View(Alldata_WD[!is.na(Alldata_WD$Materials), c('Materials', 'Materials_cat')])

table(Alldata_WD$Materials_cat) 


## Preprint
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "TIME REQUIRED|MORE WORK")] <- 'Time investment'
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "PUBLICATION OF FULL PAPER|PREVENT PUBLICATION|TOP JOURNALS|QUALIFY FOR PUBLICATIONS")] <- 'Prevents or complicates formal publishing'
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "PEER REVIEW*|UNRELIABLE|ISSUES|PEER-REVIEW*|ERRO*|POOR QUALITY|SCHOLARS|MISUSE OF WORK IN PROGRESS|SHADY PREPRINTS|SUBSTANDARD WORK|CITED WITHOUT REFLECTION|OUT OF CONTEXT|DISSEMINATED BEFORE THEIR VERACITY IS CONFIRMED|LACK OF QUALITY CONTROL|NOT FINISHED")] <- 'Misleading due to lack of peer review or unfinished work' # (e.g. public, media, other researchers (for new research or as citation)) and requires to revisit the final version (which few will do)'
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "BLIND PEER REVIEW*|DOUBLE-BLIND REVIEW*")] <- 'Interferes with blind peer reviewing' # to overwrite the simple 'peer review' above
Alldata_WD$Preprint_cat2[str_detect(Alldata_WD$Preprint, "FIRST TO PUBLISH|LOW QUALITY CAN BE EASILY DISSEMINATED|REDUCE RELIABILITY")] <- 'Lowers quality' # by increasing \'first to publish\' pressure' 
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "FASTER|SCOOP|POACHING IDEAS")] <- 'Fear of scooping' 
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "ECOLOGICAL COST")] <- 'Ecological cost'
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "BETTER USE ELSEWHERE|AREN'T SO URGENT")] <- 'Not useful'
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "BUSINESS MODEL")] <- "Loss of publishers' income"
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "UPDATING CITATIONS|MULTIPLE VERSIONS|NOT HAVING STABLE SOURCES")] <- "Problem with updating citations"
Alldata_WD$Preprint_cat2[str_detect(Alldata_WD$Preprint, "MORE DIFFICULT TO OVERVIEW|EXPONENTIAL GROWTH|OVER-PUBLICATION")] <- "Extrem growth of the litterature"

Alldata_WD$Preprint_cat[!is.na(Alldata_WD$Preprint) & is.na(Alldata_WD$Preprint_cat)] <- 'Not categorised'
Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & Alldata_WD$Preprint_cat == 'Not categorised']
Alldata_WD$Preprint_cat[!is.na(Alldata_WD$Preprint_cat2) & Alldata_WD$Preprint_cat == 'Not categorised'] <- Alldata_WD$Preprint_cat2[!is.na(Alldata_WD$Preprint_cat2) & Alldata_WD$Preprint_cat == 'Not categorised']
Alldata_WD$Preprint_cat2[!is.na(Alldata_WD$Preprint_cat2) & Alldata_WD$Preprint_cat == Alldata_WD$Preprint_cat2] <- NA
Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & Alldata_WD$Preprint_cat == 'Not categorised']


quote_WD_Preprint_1 <- tolower(Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & startsWith(Alldata_WD$Preprint,"OUR FIELD HAS SEEN A EXPONENTIAL GROWTH")==TRUE])
quote_WD_Preprint_2 <- tolower(Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & startsWith(Alldata_WD$Preprint,"DOUBLE BLIND PEER REVIEW ")==TRUE])
quote_WD_Preprint_3 <- tolower(Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & startsWith(Alldata_WD$Preprint,"NOT ALL WORKING PAPERS ARE SUITABLE")==TRUE])
quote_WD_Preprint_4 <- tolower(Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & startsWith(Alldata_WD$Preprint,"HAVING MULTIPLE VERSIONS OF THE SAME DOCUMENT")==TRUE])
quote_WD_Preprint_5 <- tolower(Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & startsWith(Alldata_WD$Preprint,"AS SEEN DURING THE COVID-19 PANDEMIC")==TRUE])
quote_WD_Preprint_6 <- tolower(Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & startsWith(Alldata_WD$Preprint,"PREPRINTS HAVE NOT YET BEEN PEER REVIEWED, SO COULD BE MISLEADING, BUT")==TRUE])
quote_WD_Preprint_7 <- tolower(Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & startsWith(Alldata_WD$Preprint,"PRE-PRINTS ARE OF COURSE")==TRUE]) # association with open peer review for archive?
quote_WD_Preprint_8 <- tolower(Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & startsWith(Alldata_WD$Preprint,"WHILE USEFUL, THE LACK OF PEER REVIEW IN PRE-PRINTS IS A POSSIBLE DOWNSIDE IN THE LONG-RUN")==TRUE])

quote_WD_Preprint_1 <- data$WhatDownsides_Preprint[!is.na(data$WhatDownsides_Preprint) & startsWith(data$WhatDownsides_Preprint, "Our field has seen a exponential growth")==TRUE]
quote_WD_Preprint_2 <- data$WhatDownsides_Preprint[!is.na(data$WhatDownsides_Preprint) & startsWith(data$WhatDownsides_Preprint, "Double blind peer review")==TRUE]
quote_WD_Preprint_3 <- data$WhatDownsides_Preprint[!is.na(data$WhatDownsides_Preprint) & startsWith(data$WhatDownsides_Preprint, "Not all working papers are suitable")==TRUE]
quote_WD_Preprint_4 <- data$WhatDownsides_Preprint[!is.na(data$WhatDownsides_Preprint) & startsWith(data$WhatDownsides_Preprint, "Having multiple versions of the same document")==TRUE]
quote_WD_Preprint_5 <- data$WhatDownsides_Preprint[!is.na(data$WhatDownsides_Preprint) & startsWith(data$WhatDownsides_Preprint, "As seen during the COVID-19 pandemic")==TRUE]
quote_WD_Preprint_6 <- data$WhatDownsides_Preprint[!is.na(data$WhatDownsides_Preprint) & startsWith(data$WhatDownsides_Preprint, "Preprints have not yet been peer reviewed")==TRUE]
quote_WD_Preprint_7 <- data$WhatDownsides_Preprint[!is.na(data$WhatDownsides_Preprint) & startsWith(data$WhatDownsides_Preprint, "Pre-prints are of course")==TRUE]
quote_WD_Preprint_8 <- data$WhatDownsides_Preprint[!is.na(data$WhatDownsides_Preprint) & startsWith(data$WhatDownsides_Preprint, "While useful, the lack of peer review")==TRUE]


table(c(Alldata_WD$Preprint_cat, Alldata_WD$Preprint_cat2))
# View(Alldata_WD[!is.na(Alldata_WD$Preprint), c('Preprint', 'Preprint_cat', 'Preprint_cat2')])


## Preregistration
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "PIPPED|STEAL*|SCOOPED|PUBLISH BEFORE YOU|COMPETITIVE|SCOOPING|EXPLOITED BY COMPETITORS|COMPETITORS")] <- 'Fear of scooping' 
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "MORE TIME|SLOW DOWN|TIME FOR RESEARCH|TIME IT TAKES|TIME CONSTRAINTS|EXTRA EFFORT|ADDITIONAL WORKLOAD|TIME-INTENSIVE")] <- 'Time investment' 
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "NO EXPERIMENTATION|NOT RELEVANT|NOT A USEFUL PRACTISE|I CAN SEE THIS BEING APPLICABLE|IRRELEVANT|NOT ALL WORK WELL SUITED|SOME METHODOLOGIES")] <- 'Not relevant for all fields' # (e.g. theoretical, mathematical research where biases are not present or in the humanities that do not follow a scientific process)' 
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "EXPLORATORY|BLUE SKIES|INDUCTIVE LED DISCOVERIES")] <- 'Impedes exploratory research' 
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "NULL FINDINGS|DON'T PUBLISH NEGATIVE FINDINGS")] <- 'Needs corresponding increase in respect for null findings' 
Alldata_WD$Prereg_cat2[str_detect(Alldata_WD$Prereg, "EVOLVING|HYPOTHESES CHANGE|WIGGLE ROOM|UPDATE PROTOCOL|ADAPT TO UNFORESEEN|FORTUITOUS AND UNPREDICTABLE|LIMITED FLEXIBILITY|LACK OF FLEXIBILITY|TAKES AWAY THE FLEXIBILITY|LIKELY TO CHANGE OVER TIME|LIMIT FLEXIBILITY")] <- 'Impedes flexibility in protocols'
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "REDUCE THE SCIENTIFIC PROCESS")] <- 'Reduce scientific process to simple binary hypothesis' 
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "IMPOSSIBLE TO ACCOUNT FOR ALL THE COMPLEXITIES OF BIG DATA ANALYSES UNTIL ONE SEES THE DATA")] <- 'Too difficult to prepare statistical plan in advance'
Alldata_WD$Prereg_cat2[str_detect(Alldata_WD$Prereg, "DETER OTHER RESEARCHERS FROM ATTEMPTING TO ADDRESS|ACTUALLY REDUCING REPRODUCTION OF THE SCIENCE")] <- 'Deter unknowing replication'
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "ITERATIVE DEVELOPMENT|IT IS ITERATIVE AND CUMULATIVE")] <- 'Iterative development of the research question in social sciences'
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "NO CLEAR FORMAT OR STANDARD")] <- 'No clear format or standards'

 
quote_WD_Prereg_1 <- data$WhatDownsides_Prereg[!is.na(data$WhatDownsides_Prereg) & startsWith(data$WhatDownsides_Prereg, "Current preregistration formats")==TRUE]
quote_WD_Prereg_2 <- data$WhatDownsides_Prereg[!is.na(data$WhatDownsides_Prereg) & startsWith(data$WhatDownsides_Prereg, "I think this is fine for controlled trials")==TRUE]
quote_WD_Prereg_3 <- data$WhatDownsides_Prereg[!is.na(data$WhatDownsides_Prereg) & startsWith(data$WhatDownsides_Prereg, "If done thoughtfully")==TRUE]
quote_WD_Prereg_4 <- data$WhatDownsides_Prereg[!is.na(data$WhatDownsides_Prereg) & startsWith(data$WhatDownsides_Prereg, "Pre-registration is not useful in")==TRUE]
quote_WD_Prereg_5 <- data$WhatDownsides_Prereg[!is.na(data$WhatDownsides_Prereg) & startsWith(data$WhatDownsides_Prereg, "At least in political science")==TRUE]
quote_WD_Prereg_6 <- data$WhatDownsides_Prereg[!is.na(data$WhatDownsides_Prereg) & startsWith(data$WhatDownsides_Prereg, "In ecology")==TRUE]
quote_WD_Prereg_7 <- data$WhatDownsides_Prereg[!is.na(data$WhatDownsides_Prereg) & startsWith(data$WhatDownsides_Prereg, "If exploratory research (no prereg")==TRUE]
quote_WD_Prereg_8 <- data$WhatDownsides_Prereg[!is.na(data$WhatDownsides_Prereg) & startsWith(data$WhatDownsides_Prereg, "Researchers should not claim ideas")==TRUE]

                                                                                 
Alldata_WD$Prereg_cat[!is.na(Alldata_WD$Prereg) & is.na(Alldata_WD$Prereg_cat)] <- 'Not categorised'
Alldata_WD$Prereg[!is.na(Alldata_WD$Prereg) & Alldata_WD$Prereg_cat == 'Not categorised']
Alldata_WD$Prereg_cat[!is.na(Alldata_WD$Prereg_cat2) & Alldata_WD$Prereg_cat == 'Not categorised'] <- Alldata_WD$Prereg_cat2[!is.na(Alldata_WD$Prereg_cat2) & Alldata_WD$Prereg_cat == 'Not categorised']
Alldata_WD$Prereg_cat2[!is.na(Alldata_WD$Prereg_cat2) & Alldata_WD$Prereg_cat == Alldata_WD$Prereg_cat2] <- NA
Alldata_WD$Prereg[!is.na(Alldata_WD$Prereg) & Alldata_WD$Prereg_cat == 'Not categorised']

table(c(Alldata_WD$Prereg_cat,Alldata_WD$Prereg_cat2))
# View(Alldata_WD[!is.na(Alldata_WD$Prereg), c('Prereg', 'Prereg_cat', 'Prereg_cat2')])



## Registered Report
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "PIPPED|STEAL*|SCOOPED|PUBLISH BEFORE YOU|COMPETITIVE|POACHING|COPY METHOD|COMPETITORS|SCOOPING|SCOOP")] <- 'Fear of scooping' 
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "NO EXPERIMENTATION|NOT RELEVANT|NOT A USEFUL PRACTISE|I CAN SEE THIS BEING APPLICABLE|IRRELEVANT|NOT ALL WORK WELL SUITED|SOME METHODOLOGIES|TYPE STUDIES|NEWER FIELD|QUALITATIVE RESEARCH DIFFERS|MY FIELD IS TYPICALLY EVALUATING|MOSTLY THEORETICAL")] <- 'Not relevant for all fields' # (e.g. theoretical, mathematical research where biases are not present or in the humanities that do not follow a scientific process)' 
Alldata_WD$RegRep_cat2[str_detect(Alldata_WD$RegRep, "EVOLVING|HYPOTHESES CHANGE|WIGGLE ROOM|UPDATE PROTOCOL|ADAPT TO UNFORESEEN|FORTUITOUS AND UNPREDICTABLE|LIMITED FLEXIBILITY|LACK OF FLEXIBILITY|TAKES AWAY THE FLEXIBILITY|TAKES AWAY FLEXIBILITY|LIKELY TO CHANGE OVER TIME|UNCERTAINTY IN RESEARCH AHEAD OF TIME|EXPERIMENTAL DESIGN AN PLAN OFTEN CHANGES AS RESEARCH PROGRESS|LIMIT FLEXIBILITY")] <- 'Impedes flexibility in protocols'
Alldata_WD$RegRep_cat3[str_detect(Alldata_WD$RegRep, "MORE TIME|SLOW DOWN|TIME FOR RESEARCH|SLOWS|TIME COST|EXTRA EFFORT REQUIRED|A LOT OF WORK")] <- 'Time investment' 
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "TIMESCALE|DELAY IN FEEDBACK|DELAYS|DELAY THE PROJECT TIMELINE")] <- 'Delays start of research' # Challenging timescale for ECR under short term contracts
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED")] <- 'Intellectual property concerns' #including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "EXPLORATORY|BLUE SKIES")] <- 'Impedes exploratory research' 
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "ARE INDEPENDENT OF THE VALIDITY OF THE UNDERLYING SCIENTIFIC")] <- 'Publishing of unclear results because the study does not work despite validity of method'
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "NOT ALL PEER REVIEWERS ARE CORRECT")] <- 'Poor peer review comments, if opened to public, can potentially damage the paper'

quote_WD_RegRep_1 <- data$WhatDownsides_RegRep[!is.na(data$WhatDownsides_RegRep) & startsWith(data$WhatDownsides_RegRep, "Qualitative research differs")==TRUE]
quote_WD_RegRep_2 <- data$WhatDownsides_RegRep[!is.na(data$WhatDownsides_RegRep) & startsWith(data$WhatDownsides_RegRep, "Delay in feedback")==TRUE]
quote_WD_RegRep_3 <- data$WhatDownsides_RegRep[!is.na(data$WhatDownsides_RegRep) & startsWith(data$WhatDownsides_RegRep, "I'm not sure that")==TRUE]
quote_WD_RegRep_4 <- data$WhatDownsides_RegRep[!is.na(data$WhatDownsides_RegRep) & startsWith(data$WhatDownsides_RegRep, "If exploratory research")==TRUE]
quote_WD_RegRep_5 <- data$WhatDownsides_RegRep[!is.na(data$WhatDownsides_RegRep) & startsWith(data$WhatDownsides_RegRep, "For large-scale psychology")==TRUE]
quote_WD_RegRep_6 <- data$WhatDownsides_RegRep[!is.na(data$WhatDownsides_RegRep) & startsWith(data$WhatDownsides_RegRep, "Science is not linear")==TRUE]


Alldata_WD$RegRep_cat[!is.na(Alldata_WD$RegRep) & is.na(Alldata_WD$RegRep_cat)] <- 'Not categorised'
Alldata_WD$RegRep[!is.na(Alldata_WD$RegRep) & Alldata_WD$RegRep_cat == 'Not categorised']
Alldata_WD$RegRep_cat[!is.na(Alldata_WD$RegRep_cat2) & Alldata_WD$RegRep_cat == 'Not categorised'] <- Alldata_WD$RegRep_cat2[!is.na(Alldata_WD$RegRep_cat2) & Alldata_WD$RegRep_cat == 'Not categorised']
Alldata_WD$RegRep_cat2[!is.na(Alldata_WD$RegRep_cat2) & Alldata_WD$RegRep_cat == Alldata_WD$RegRep_cat2] <- NA
Alldata_WD$RegRep[!is.na(Alldata_WD$RegRep) & Alldata_WD$RegRep_cat == 'Not categorised']
Alldata_WD$RegRep_cat[!is.na(Alldata_WD$RegRep_cat3) & Alldata_WD$RegRep_cat == 'Not categorised'] <- Alldata_WD$RegRep_cat3[!is.na(Alldata_WD$RegRep_cat3) & Alldata_WD$RegRep_cat == 'Not categorised']
Alldata_WD$RegRep_cat3[!is.na(Alldata_WD$RegRep_cat3) & Alldata_WD$RegRep_cat == Alldata_WD$RegRep_cat3] <- NA
Alldata_WD$RegRep[!is.na(Alldata_WD$RegRep) & Alldata_WD$RegRep_cat == 'Not categorised']

table(c(Alldata_WD$RegRep_cat, Alldata_WD$RegRep_cat2, Alldata_WD$RegRep_cat3))
# View(Alldata_WD[!is.na(Alldata_WD$RegRep), c('RegRep', 'RegRep_cat', 'RegRep_cat2','RegRep_cat3')])




# Create list for checking categories -----

### function needs updating if more cat2 and cat3 categories are created !!!! 

a_pgrdata_OB <- create_list_for_checking_cat(subset(Alldata_OB[Alldata_OB$Subdataset == 'pgrdata',],select=c(-Subdataset)))
a_allstaffdata_OB <- create_list_for_checking_cat(subset(Alldata_OB[Alldata_OB$Subdataset != 'pgrdata',],select=c(-Subdataset)))
a_Alldata_OB <- create_list_for_checking_cat(subset(Alldata_OB,select=c(-Subdataset)))

a_pgrdata_WD <- create_list_for_checking_cat(subset(Alldata_WD[Alldata_WD$Subdataset == 'pgrdata',],select=c(-Subdataset)))
a_allstaffdata_WD <- create_list_for_checking_cat(subset(Alldata_WD[Alldata_WD$Subdataset != 'pgrdata',],select=c(-Subdataset)))
a_Alldata_WD <- create_list_for_checking_cat(subset(Alldata_WD,select=c(-Subdataset)))


## to print list for someone to check recoding categories
list_for_checking_cat <- rbind(cbind(a_Alldata_OB, Question = 'OB'), cbind(a_Alldata_WD, Question = 'WD'))

#write.csv(list_for_checking_cat, file='list_for_checking_cat.csv')

# Create pivot tables from list for checking categories
pgrdata_OB_table <- create_pivot_table_from_list_for_checking_cat(a_pgrdata_OB, "Other barriers to adoption of ORPs")
allstaffdata_OB_table <- create_pivot_table_from_list_for_checking_cat(a_allstaffdata_OB, "Other barriers to adoption of ORPs")
Alldata_OB_table <- create_pivot_table_from_list_for_checking_cat(a_Alldata_OB, "Other barriers to adoption of ORPs")

pgrdata_WD_table <- create_pivot_table_from_list_for_checking_cat(a_pgrdata_WD, "What downsides to adoption of ORPs")
allstaffdata_WD_table <- create_pivot_table_from_list_for_checking_cat(a_allstaffdata_WD, "What downsides to adoption of ORPs")
Alldata_WD_table <- create_pivot_table_from_list_for_checking_cat(a_Alldata_WD, "What downsides to adoption of ORPs")


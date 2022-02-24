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
Alldata_OB$OA_cat[str_detect(Alldata_OB$OA, "HTTP://OPENACCESS.OX.AC.UK/")] <- 'University website unclear, it takes time to find out which journals is OA'
Alldata_OB$OA_cat2[str_detect(Alldata_OB$OA, "ADMIN PEOPLE|CONFUSION OVER POLICY EXPECTATIONS")] <- 'Policy changing rapidly, confusion, University administration unaware of researchers needs'
Alldata_OB$OA_cat2[str_detect(Alldata_OB$OA, "BOOK PUBLISHER FOR SALES LOST")] <- 'Book publishers sales loss'

quote_OB_OA_1 <- tolower("FUNDING FOR OPEN ACCESS PUBLISHING IS INSUFFICIENT, OPAQUE AND WITH PATCHY COVERAGE. THERE IS ONLY HALF A POLICY - THAT WE MUST PUBLISH BY OPEN ACCESS, BUT WITHOUT THE OTHER HALF THAT WOULD ACTUALLY RESOURCE THOSE OPEN ACCESS FEES. THIS LEAVES WEALTHY UNIVERSITIES, DEPARTMENTS, GROUPS AND EVEN INDIVIDUALS AT A COMPETITIVE ADVANTAGE OVER OTHERS ACTING AS A BIAS OVER WHAT IS ABLE TO BE FORMALLY PUBLISHED.") 
quote_OB_OA_2 <- tolower("THE COST OF OPEN ACCESS PUBLISHING IS AN ADDITIONAL BARRIER, AS IS THE CULTURE OF PUBLISHING IN TOP JOURNALS, WHICH TEND TO ONLY BE OPEN ACCESS IF A LARGE FEE IS PAID.")
quote_OB_OA_3 <- tolower("EVEN CO-CALLED 'ACADEMIC PUBLISHERS' (LIKE THE APS OR IOP) NOW CHARGE THE AUTHORS LARGE SUMS FOR MAKING PUBLICATIONS OA - AND THE UNIVERSITY/RESEARCH COUNCILS ARE COMPLICIT IN SUPPORTING SUCH PROFIT-MAKING PRACTICES. NEW NON-PROFIT OA JOURNALS SET UP BY ACADEMICS FOR ACADEMICS WHICH SIMPLY ASK FOR THE ACTUAL, NOMINAL COSTS OF PUBLISHING ARE NOT PRIORITISED FOR SUPPORT.") 

Alldata_OB$OA_cat[!is.na(Alldata_OB$OA) & is.na(Alldata_OB$OA_cat)] <- 'Not categorised'
Alldata_OB$OA[!is.na(Alldata_OB$OA_cat) & Alldata_OB$OA_cat == 'Not categorised']
Alldata_OB$OA_cat[!is.na(Alldata_OB$OA_cat2) & Alldata_OB$OA_cat == 'Not categorised'] <-  Alldata_OB$OA_cat2[!is.na(Alldata_OB$OA_cat2) & Alldata_OB$OA_cat == 'Not categorised']
Alldata_OB$OA_cat2[!is.na(Alldata_OB$OA_cat2) & Alldata_OB$OA_cat == Alldata_OB$OA_cat2] <- NA
Alldata_OB$OA[!is.na(Alldata_OB$OA_cat) & Alldata_OB$OA_cat == 'Not categorised']
# View(Alldata_OB[,c('OA','OA_cat','OA_cat2')])

table(c(Alldata_OB$OA_cat, Alldata_OB$OA_cat2))


## Data
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "MANAGING DATA|TB OF DATA")] <- 'Difficult resource management and lack of metadata standards'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|ETHICAL REGULATIONS|MUST NOT BE SHARED|CONFIDENTIALITY|IDENTIFIABILITY|CONSENT")] <- 'Ethical concerns'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "AUTHORITY|INDUSTRY|COMMERCIAL*|PARTNERS|POLITICS")] <- 'Resource not owned'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "OEUVERS")] <- 'Resource not always digital'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "FUNDING FOR DATA CLEANING|ALLOCATION OF RELEVANT RESEARCHERS|EFFORT OF COLLECTING AND PROCESSING THE DATA IS IGNORED")] <- 'Support/funding for preparing resource to share'
Alldata_OB$Data_cat2[str_detect(Alldata_OB$Data, "LACK OF NORMS ON AN APPROPRIATE EMBARGO PERIOD|FAILURE TO BENEFIT FROM OUR OWN HARD WORK|DATA MAY BE USED FOR OTHER PUBLICATIONS")] <- 'Original effort/cost of resource collection is ignored'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "ENABLE STORAGE OF DATA")] <- 'Funding to store resource'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "HOW USEFUL")] <- 'Not useful, no reusability'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "USER-UNFRIENDLY")] <- 'Repositories not user-friendly'

quote_OB_Data_1 <- tolower("IT IS NOT TRIVIAL TO RECONCILE INFORMATION GOVERNANCE, DATA LAW AND ETHICAL APPROVAL WITH OPEN PUBLICATION OF DATA.  THE CURRENT LEGAL DEFINITION OF 'ANONYMISED' DATA IS AN UNREACHABLY HIGH STANDARD.")
quote_OB_Data_2 <- tolower("LACK OF NORMS ON AN APPROPRIATE EMBARGO PERIOD FOR NEWLY COLLECTED DATA")

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

quote_OB_Code_1 <- tolower("THE MAIN BARRIER IS TIME NEEDED TO LEARN HOW TO BEST SET IT UP. ESPECIALLY GIVEN THE LACK OF INCENTIVES TO DO SO (I.E. TAKES THE TIME AWAY FROM PAPER AND GRANT WRITING THAT ARE MUCH MORE VALUED IN CAREER PROGRESSION).")

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

Alldata_OB$Materials_cat[!is.na(Alldata_OB$Materials) & is.na(Alldata_OB$Materials_cat)] <- 'Not categorised'
Alldata_OB$Materials[!is.na(Alldata_OB$Materials) & Alldata_OB$Materials_cat == 'Not categorised']
# View(Alldata_OB[,c('Materials','Materials_cat')])

quote_OB_Material_1 <- tolower("I FIND IT HARD TO FIND TIME TO CATALOGUE MATERIAL FOR DISCOVERABLE STORAGE WITH ALL OTHER DUTIES.")

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

quote_OB_Preprint_1 <- tolower("NOT SURE WHEN IT IS A PROBLEM FOR A JOURNAL AND WHEN CAUSES NO PROBLEMS")

table(Alldata_OB$Preprint_cat)


## Preregistration
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "PRIOR TO PUBLICATION|UNSCRUPULOUS APPROPRIATION OF IDEAS|AN UNKNOWN REGISTER")] <- 'Fear of scooping'
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "PILOT")] <- 'Lack of funding for pilot studies'
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "SHOULD NOT BE APPLIED UNIVERSALLY|DISCIPLINE|NOT ALL WORK IS WELL SUITED|HARD|UNREALISTIC|MADNESS")] <- 'Not applicable to all disciplines, or difficult'
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "FUNDING")] <- 'Depend on funding body'

quote_OB_Prereg_1 <- tolower("IN MY FIELD WE HAVE TO APPLY FOR BEAM TIME AT VERY LARGE FACILITIES. THIS INVOLVES GETTING THE EXPERIMENT APPROVED BY AN ACCESS PANEL, STATING CLEAR OBJECTIVES ETC. HOWEVER, WHEN IT COMES TO THE EXPERIMENT ITSELF THE REALITY IS OFTEN RATHER DIFFERENT (THINGS DON'T WORK, SAMPLES ARE NOT READY, THE FACILITY DOES NOT PERFORM AS IT SHOULD, THE PROPOSED METHOD IS TOO DIFFICULT IN PRACTICE, OR THE RESULTS LOOK VERY DIFFERENT FROM EXPECTATIONS. IN SUCH CASES -- WHICH OCCUR MORE OFTEN THAN NOT -- WE HAVE TO FOLLOW OUR NOSE, AND CHANGE THE OBJECTIVES AND METHODS AS THE EXPERIMENT PROGRESSES. IT WOULD BE MADNESS TO DO ANYTHING ELSE -- AND EXPENSIVE TOO, WHEN THE EXPERIMENTS COST AROUND £0.5M TO RUN.")

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

quote_OB_RegRep_1 <- tolower(Alldata_OB$RegRep[!is.na(Alldata_OB$RegRep) & startsWith(Alldata_OB$RegRep,"IN MY FIELD (EXPERIMENTAL MOLECULAR BIOLOGY) FOR")==TRUE])

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
Cells_to_fillup_manually <- Alldata_WD[unique(c(
  which(str_detect(Alldata_WD$Data, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Code, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Materials, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Preprint, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Prereg, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$RegRep, "AS ABOVE|AS FOR|AS IN|SAME AS|\\^")))),]

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
Alldata_WD$OA_cat <- NA
Alldata_WD$OA_cat2 <- NA
Alldata_WD$Code_cat <- NA
Alldata_WD$Code_cat2 <- NA
Alldata_WD$Materials_cat <- NA
Alldata_WD$Preprint_cat <- NA
Alldata_WD$Prereg_cat <- NA
Alldata_WD$Prereg_cat2 <- NA
Alldata_WD$RegRep_cat <- NA
Alldata_WD$RegRep_cat2 <- NA
Alldata_WD$RegRep_cat3 <- NA

### OA
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "HARM*|INAPPROPRIATE")] <- 'No control over validity of reuse, misrepresentation, misuse'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANC*|PAY|CHARGES|EXPENSE")] <- 'Financial cost' # need to be first to ahve a couple of overwrites below
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "JOURNAL INCOME|PRODUCTION|INCOME FROM JOURNAL SUBSCRIPTIONS|DAMAGING TO PUBLISHERS|DISINCENTIVE FOR PUBLISHERS|LEARNED SOCIETIES|PUBLISHERS KEEP TELLING US|DISINCENTIVISES PURCHASES OF PRINT|THREATENS THE FINANCING OF PUBLISHERS|OUT OF BUSINESS")] <- "Loss of scholarly societies, authors, or publishers' income (e.g. for books)" # need to find other means of journal production"
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "REDUCTION IN OVERALL QUALITY|NOT ALWAYS OF PARTICULARLY HIGH QUALITY|QUALITY OF PUBLICATION|RIGOR*|POOR-QUALITY|LACK OF QUALITY CONTROL|REDUCE THE QUALITY OF PEER REVIEW IN JOURNALS|LOWERING THE QUALITY|MAY REDUCE EDITORIAL INPUT")] <- 'Lowers quality' # reduce quality of peer review if journal paid
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "OPTIONS|LIMITS THE JOURNALS|IMPORTANT JOURNALS|REPUTABLE PRESSES|LESS PRESTIGIOUS")] <- 'Fewer (prestigious) journal options'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "OPINION|HARMFUL APPLICATIONS|HARASSMENT")] <- 'Ethical, safety, or security concerns'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "ADMINISTRATIVE|SLOW DOWN PUBLICATIONS")] <- 'Too much administrative time'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "PREDATORY|USE THIS TREND AS AN OPPORTUNITY TO INCREASE THEIR REVENUE|A WAY TO GET MORE MONEY|PUBLISHERS HAVE CARTE BLANCHE|PUBLISH TOO MANY PAPERS")] <- 'Creates predatory behaviours'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "FUNDING AVAILABILITY COULD BECOME MORE IMPORTANT THAN QUALITY|EXCLUSI*|EXCLUD*|INEQUALIT*|POORER|PEOPLE WITH LOTS OF MONEY|LOW INCOME REGIONS|WEALTHY|WHEALTIER|MORE AFFLUENT INSTITUTIONS GET PREFERENTIAL|COMPETITIVE ADVANTAGE|DEVELOPING COUNTRIES OUT|DETRIMENTAL FOR DIVERSITY|RESOURCE-LIMITED RESEARCHERS|MANY RESEARCHERS CANNOT AFFORD|MINORITISED GROUPS|OTHERS WITHOUT DEDICATED FUNDING|RESEARCHERS WITHOUT FUNDING|ONLY THOSE WITH FUNDING|HARDER FOR THOSE|MIDDLE-INCOME|TO SOME GROUPS|ONLY FAVOURS|POORLY|DISCRIMINATES|THIS IMPACTS MORE|HISTORICALLY DISADVANTAGED AREAS|DETRIMENTAL TO SUCH FIELDS")] <- 'Creates inequalities between researchers/fields/institutions with/without access to funding for APC'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "PREFER HAVING THE HARD COPY OF A BOOK")] <- 'OA requires electronical versions, prefer hard copy of books'


quote_WD_OA_1 <- tolower(Alldata_WD$OA[!is.na(Alldata_WD$OA) & startsWith(Alldata_WD$OA,"JOURNALS HAVE PASSED TRULY EXCESSIVE COSTS OF PUBLICATION ON TO RESEARCHERS. THIS DIVERTS")==TRUE])
quote_WD_OA_2 <- tolower(Alldata_WD$OA[!is.na(Alldata_WD$OA) & startsWith(Alldata_WD$OA,"LOWERS BARRIER TO ARTICLE SUBMISSION (IF PEOPLE JUST PUBLISH TO OPEN ACCESS WEBSITES E.G. ARXIV)")==TRUE])
quote_WD_OA_3 <- tolower(Alldata_WD$OA[!is.na(Alldata_WD$OA) & startsWith(Alldata_WD$OA,"GOLD OPEN ACCESS ARTICLES ARE READ AND CITED MORE")==TRUE])
quote_WD_OA_4 <- tolower(Alldata_WD$OA[!is.na(Alldata_WD$OA) & startsWith(Alldata_WD$OA,"THE MOVE TOWARDS OPEN SCIENCE PUBLICATIONS")==TRUE])
quote_WD_OA_5 <- tolower(Alldata_WD$OA[!is.na(Alldata_WD$OA) & startsWith(Alldata_WD$OA,"IF THE FOCUS IS ON GOLD OA")==TRUE])
quote_WD_OA_6 <- tolower(Alldata_WD$OA[!is.na(Alldata_WD$OA) & startsWith(Alldata_WD$OA,"I WORK IN A FIELD THAT DOES NOT ATTRACT A LOT OF EXTERNAL RESEARCH FUNDING, NOR DOES IT ESPECIALLY NEED IT")==TRUE])
quote_WD_OA_7 <- tolower(Alldata_WD$OA[!is.na(Alldata_WD$OA) & startsWith(Alldata_WD$OA,"OPEN ACCESS PUBLICATION IS A LAUDABLE GOAL")==TRUE])

Alldata_WD$OA_cat[!is.na(Alldata_WD$OA) & is.na(Alldata_WD$OA_cat)] <- 'Not categorised'
Alldata_WD$OA[!is.na(Alldata_WD$OA) & Alldata_WD$OA_cat == 'Not categorised']
# View(Alldata_WD[,c('OA','OA_cat')])

table(Alldata_WD$OA_cat)

## Data
Alldata_WD$OA_cat[str_detect(Alldata_WD$Data, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN|NOT WITHIN THE SCOPE")] <- 'Time investment' # more work, not valued for career, significant burden for qualitative researchers
Alldata_WD$OA_cat2[str_detect(Alldata_WD$Data, "CONTINUITY")] <- 'Challenges around continuity of ownership (e.g. for longitudinal dataset)'
Alldata_WD$OA_cat[str_detect(Alldata_WD$Data, "ANONYM*|SENSITIVE DATA|PRIVA*|PARTICIPANT DATA|DATA PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|TRICKY|DPA")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
Alldata_WD$OA_cat[str_detect(Alldata_WD$Data, "LICENCES|LICENSES|FORMATS")] <- 'Proprietary file format'
Alldata_WD$OA_cat[str_detect(Alldata_WD$Data, "HARMFUL|MALICIOUS|MISUSE|MALIGN")] <- 'No control over validity of reuse, misrepresentation, misuse'
Alldata_WD$OA_cat[str_detect(Alldata_WD$Data, "PIPPED|STEALING|SCOOPED")] <- 'Fear of scooping'
Alldata_WD$OA_cat2[str_detect(Alldata_WD$Data, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|CREDIT|COPYRIGHT*|PROTECTION OF IP")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation'
Alldata_WD$OA_cat[str_detect(Alldata_WD$Data, "FIRST TO PUBLISH")] <- 'Lowers quality' # by increasing \'first to publish\' pressure' 
Alldata_WD$OA_cat[str_detect(Alldata_WD$Data, "NO NORM FOR CITATION")] <- 'No norm for citation'

Alldata_WD$OA_cat[!is.na(Alldata_WD$Data) & is.na(Alldata_WD$OA_cat)] <- 'Not categorised'
Alldata_WD$Data[!is.na(Alldata_WD$Data) & Alldata_WD$OA_cat == 'Not categorised']
Alldata_WD$OA_cat[!is.na(Alldata_WD$OA_cat2) & Alldata_WD$OA_cat == 'Not categorised'] <-  Alldata_WD$OA_cat2[!is.na(Alldata_WD$OA_cat2) & Alldata_WD$OA_cat == 'Not categorised']
Alldata_WD$OA_cat2[!is.na(Alldata_WD$OA_cat2) & Alldata_WD$OA_cat == Alldata_WD$OA_cat2] <- NA
Alldata_WD$Data[!is.na(Alldata_WD$Data) & Alldata_WD$OA_cat == 'Not categorised']

table(c(Alldata_WD$OA_cat, Alldata_WD$OA_cat2))
#View(Alldata_WD[!is.na(Alldata_WD$Data),c('Data', 'OA_cat','OA_cat2')])


## Code
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "POOR QUALITY|REVIEWED AND TESTED")] <- 'Lowers quality' # by propagating unreviewed and untested code
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "LONG TIME|MORE WORK|WORKLOAD|OVERHEAD|BURDEN|NOT WITHIN THE SCOPE|SLOW DOWN")] <- 'Time investment'
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|HARMFUL")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "FIRST TO PUBLISH")] <- 'Lowers quality' # by increasing \'first to publish\' pressure
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "MALICIOUS|MISUSE|INAPPRORPIATE|NOT UNDERSTAND|NUANCE")] <- 'No control over validity of reuse, misrepresentation, misuse'
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "REDUCE THE NUMBER OF APPROACHES USED")] <- 'Reduce diversity of approaches'
Alldata_WD$Code_cat2[str_detect(Alldata_WD$Code, "FIND PEOPLE TO REVIEW THE CODE")] <- 'No time or expertise to review code'
Alldata_WD$Code_cat[str_detect(Alldata_WD$Code, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED|COMMERCIAL SENSITIVITY|PROTECTION OF IP")] <- 'Intellectual property concerns' # (including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'

Alldata_WD$Code_cat[!is.na(Alldata_WD$Code) & is.na(Alldata_WD$Code_cat)] <- 'Not categorised'
Alldata_WD$Code[!is.na(Alldata_WD$Code) & Alldata_WD$Code_cat == 'Not categorised']
Alldata_WD$Code_cat[!is.na(Alldata_WD$Code_cat2) & Alldata_WD$Code_cat == 'Not categorised'] <- Alldata_WD$Code_cat2[!is.na(Alldata_WD$Code_cat2) & Alldata_WD$Code_cat == 'Not categorised']
Alldata_WD$Code_cat2[!is.na(Alldata_WD$Code_cat2) & Alldata_WD$Code_cat == Alldata_WD$Code_cat2] <- NA
Alldata_WD$Code[!is.na(Alldata_WD$Code) & Alldata_WD$Code_cat == 'Not categorised']

table(c(Alldata_WD$Code_cat, Alldata_WD$Code_cat2))
#View(Alldata_WD[!is.na(Alldata_WD$Code), c('Code', 'Code_cat', 'Code_cat2')])

## Materials
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|RADIOACTIVE")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED")] <- 'Intellectual property concerns' # (including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "FIRST TO PUBLISH")] <- 'Lowers quality'# by increasing \'first to publish\' pressure
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "PIPPED|STEALING|SCOOPED|PUBLISH PAPERS FAST")] <- 'Fear of scooping' # leading to loss of career prospect
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "HARMFUL|MALICIOUS|MISUSE|INAPPRORPIATE|NOT UNDERSTAND|NUANCE")] <- 'No control over validity of reuse, misrepresentation, misuse'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "RETURNED")] <- 'Resource not owned' # (e.g. archelogical artifacts)
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN")] <- 'Time investment' # more work, not valued for career, significant burden for qualitative researchers
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "PLATFORMS")] <- 'Unusable if not from open source platforms' # 
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "LICENCES|LICENSES")] <- 'Resource not own'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "NATIONAL POLICIES")] <- 'Law against sharing specific material'
Alldata_WD$Materials_cat[str_detect(Alldata_WD$Materials, "FRAGILE")] <- 'Impractical'

Alldata_WD$Materials_cat[!is.na(Alldata_WD$Materials) & is.na(Alldata_WD$Materials_cat)] <- 'Not categorised'
Alldata_WD$Materials[!is.na(Alldata_WD$Materials) & Alldata_WD$Materials_cat == 'Not categorised']

table(Alldata_WD$Materials_cat) 


## Preprint
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "TIME REQUIRED")] <- 'Time investment'
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "PUBLICATION OF FULL PAPER|PREVENT PUBLICATION|TOP JOURNALS")] <- 'Prevents or complicates formal publishing'
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "PEER REVIEW*|UNRELIABLE|ISSUES|PEER-REVIEW*|ERRO*|POOR QUALITY|SCHOLARS")] <- 'Misleading due to lack of peer review' # (e.g. public, media, other researchers (for new research or as citation)) and requires to revisit the final version (which few will do)'
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "BLIND PEER REVIEW*")] <- 'Interferes with blind peer reviewing' # to overwrite the simple 'peer review' above
Alldata_WD$Preprint_cat2[str_detect(Alldata_WD$Preprint, "FIRST TO PUBLISH")] <- 'Lowers quality' # by increasing \'first to publish\' pressure' 
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "FASTER")] <- 'Fear of scooping' 
Alldata_WD$Preprint_cat[str_detect(Alldata_WD$Preprint, "ECOLOGICAL COST")] <- 'Ecological cost'

Alldata_WD$Preprint_cat[!is.na(Alldata_WD$Preprint) & is.na(Alldata_WD$Preprint_cat)] <- 'Not categorised'
Alldata_WD$Preprint[!is.na(Alldata_WD$Preprint) & Alldata_WD$Preprint_cat == 'Not categorised']

table(Alldata_WD$Preprint_cat)

## Preregistration
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "PIPPED|STEAL*|SCOOPED|PUBLISH BEFORE YOU|COMPETITIVE")] <- 'Fear of scooping' 
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "MORE TIME|SLOW DOWN|TIME FOR RESEARCH|TIME IT TAKES")] <- 'Time investment' 
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "NO EXPERIMENTATION|NOT RELEVANT")] <- 'Not relevant for all fields' # (e.g. theoretical, mathematical research where biases are not present or in the humanities that do not follow a scientific process)' 
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "EXPLORATORY|BLUE SKIES")] <- 'Impedes exploratory research' 
Alldata_WD$Prereg_cat[str_detect(Alldata_WD$Prereg, "NULL FINDINGS")] <- 'Needs corresponding increase in respect for null findings' 
Alldata_WD$Prereg_cat2[str_detect(Alldata_WD$Prereg, "EVOLVING|HYPOTHESES CHANGE|WIGGLE ROOM|UPDATE PROTOCOL|ADAPT TO UNFORESEEN|FORTUITOUS AND UNPREDICTABLE")] <- 'Impedes flexibility in protocols'

Alldata_WD$Prereg_cat[!is.na(Alldata_WD$Prereg) & is.na(Alldata_WD$Prereg_cat)] <- 'Not categorised'
Alldata_WD$Prereg[!is.na(Alldata_WD$Prereg) & Alldata_WD$Prereg_cat == 'Not categorised']
Alldata_WD$Prereg_cat[!is.na(Alldata_WD$Prereg_cat2) & Alldata_WD$Prereg_cat == 'Not categorised'] <- Alldata_WD$Prereg_cat2[!is.na(Alldata_WD$Prereg_cat2) & Alldata_WD$Prereg_cat == 'Not categorised']
Alldata_WD$Prereg_cat2[!is.na(Alldata_WD$Prereg_cat2) & Alldata_WD$Prereg_cat == Alldata_WD$Prereg_cat2] <- NA

Alldata_WD$Prereg[!is.na(Alldata_WD$Prereg) & Alldata_WD$Prereg_cat == 'Not categorised']

table(c(Alldata_WD$Prereg_cat,Alldata_WD$Prereg_cat2))


## Registered Report
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "PIPPED|STEAL*|SCOOPED|PUBLISH BEFORE YOU|COMPETITIVE|POACHING|COPY METHOD")] <- 'Fear of scooping' 
Alldata_WD$RegRep_cat2[str_detect(Alldata_WD$RegRep, "EVOLVING|HYPOTHESES CHANGE|WIGGLE ROOM|UPDATE PROTOCOL|ADAPT TO UNFORESEEN|FORTUITOUS AND UNPREDICTABLE")] <- 'Impedes flexibility in protocols' # 'the understanding of data (e.g. longitudinal data) or the developement of hypothesis for an experiment are evolving, and this process is valuable, it should not be forced into a preregistration'
Alldata_WD$RegRep_cat3[str_detect(Alldata_WD$RegRep, "MORE TIME|SLOW DOWN|TIME FOR RESEARCH|SLOWS|TIME COST")] <- 'Time investment' 
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "TIMESCALE")] <- 'Challenging timescale for ECR under short term contracts' 
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED")] <- 'Intellectual property concerns' #including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
Alldata_WD$RegRep_cat[str_detect(Alldata_WD$RegRep, "EXPLORATORY|BLUE SKIES")] <- 'Impedes exploratory research' 


Alldata_WD$RegRep_cat[!is.na(Alldata_WD$RegRep) & is.na(Alldata_WD$RegRep_cat)] <- 'Not categorised'
Alldata_WD$RegRep[!is.na(Alldata_WD$RegRep) & Alldata_WD$RegRep_cat == 'Not categorised']
Alldata_WD$RegRep_cat[!is.na(Alldata_WD$RegRep_cat2) & Alldata_WD$RegRep_cat == 'Not categorised'] <- Alldata_WD$RegRep_cat2[!is.na(Alldata_WD$RegRep_cat2) & Alldata_WD$RegRep_cat == 'Not categorised']
Alldata_WD$RegRep_cat2[!is.na(Alldata_WD$RegRep_cat2) & Alldata_WD$RegRep_cat == Alldata_WD$RegRep_cat2] <- NA
Alldata_WD$RegRep[!is.na(Alldata_WD$RegRep) & Alldata_WD$RegRep_cat == 'Not categorised']
Alldata_WD$RegRep_cat[!is.na(Alldata_WD$RegRep_cat3) & Alldata_WD$RegRep_cat == 'Not categorised'] <- Alldata_WD$RegRep_cat3[!is.na(Alldata_WD$RegRep_cat3) & Alldata_WD$RegRep_cat == 'Not categorised']
Alldata_WD$RegRep_cat3[!is.na(Alldata_WD$RegRep_cat3) & Alldata_WD$RegRep_cat == Alldata_WD$RegRep_cat3] <- NA
Alldata_WD$RegRep[!is.na(Alldata_WD$RegRep) & Alldata_WD$RegRep_cat == 'Not categorised']

table(c(Alldata_WD$RegRep_cat, Alldata_WD$RegRep_cat2, Alldata_WD$RegRep_cat3))





# Create list for checking categories -----
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
pgrdata_OB_table <- create_pivot_table_from_list_for_checking_cat(a_pgrdata_OB)
allstaffdata_OB_table <- create_pivot_table_from_list_for_checking_cat(a_allstaffdata_OB)
Alldata_OB_table <- create_pivot_table_from_list_for_checking_cat(a_Alldata_OB)

pgrdata_WD_table <- create_pivot_table_from_list_for_checking_cat(a_pgrdata_WD)
allstaffdata_WD_table <- create_pivot_table_from_list_for_checking_cat(a_allstaffdata_WD)
Alldata_WD_table <- create_pivot_table_from_list_for_checking_cat(a_Alldata_WD)


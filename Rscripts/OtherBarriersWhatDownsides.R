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
  which(str_detect(Alldata_OB$Data, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_OB$Code, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_OB$Materials, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_OB$Preprint, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_OB$Prereg, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_OB$RegRep, "AS FOR|AS IN|SAME AS|\\^")))),]


## categorise barriers
Alldata_OB$OA_cat <- NA
Alldata_OB$Data_cat <- NA
Alldata_OB$Code_cat <- NA
Alldata_OB$Materials_cat <- NA
Alldata_OB$Preprint_cat <- NA
Alldata_OB$Prereg_cat <- NA
Alldata_OB$RegRep_cat <- NA

### OA
Alldata_OB$OA[!is.na(Alldata_OB$OA)]
Alldata_OB$OA_cat[str_detect(Alldata_OB$OA, "INDUSTRY")] <- 'Resource not owned'
Alldata_OB$OA_cat[str_detect(Alldata_OB$OA, c("EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANC*|PAY|CHARGES"))] <- 'Financial cost'
Alldata_OB$OA_cat[str_detect(Alldata_OB$OA, "JOURNAL QUALITY")] <- 'Lower quality'

Alldata_OB$OA_cat[!is.na(Alldata_OB$OA) & is.na(Alldata_OB$OA_cat)] <- 'Not categorised'
Alldata_OB$OA[!is.na(Alldata_OB$OA_cat) & Alldata_OB$OA_cat == 'Not categorised']

table(Alldata_OB$OA_cat)
table(Alldata_OB$OA_cat[Alldata_OB$Subdataset == 'pgrdata'])

## Data
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "MANAGING DATA")] <- 'Difficult resource management and lack of metadata standards'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA")] <- 'Ethical concerns'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "AUTHORITY|INDUSTRY|COMMERCIAL*")] <- 'Resource not owned'
Alldata_OB$Data_cat[str_detect(Alldata_OB$Data, "OEUVERS")] <- 'Resource not always digital'

Alldata_OB$Data_cat[!is.na(Alldata_OB$Data) & is.na(Alldata_OB$Data_cat)] <- 'Not categorised'
Alldata_OB$Data[!is.na(Alldata_OB$Data_cat) & Alldata_OB$Data_cat == 'Not categorised']

table(Alldata_OB$Data_cat)

## Code
Alldata_OB$Code_cat[str_detect(Alldata_OB$Code, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
Alldata_OB$Code_cat[str_detect(Alldata_OB$Code, "TIME|LOT OF WORK")] <- 'Time investment'

Alldata_OB$Code_cat[!is.na(Alldata_OB$Code) & is.na(Alldata_OB$Code_cat)] <- 'Not categorised'
Alldata_OB$Code[!is.na(Alldata_OB$Code) & Alldata_OB$Code_cat == 'Not categorised']

table(Alldata_OB$Code_cat)

## Materials
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "MANAGING DATA")] <- 'Difficult resource management and lack of metadata standards'
Alldata_OB$Materials_cat[str_detect(Alldata_OB$Materials, "LIBRARIES")] <- 'Resource not always digital'

Alldata_OB$Materials_cat[!is.na(Alldata_OB$Materials) & is.na(Alldata_OB$Materials_cat)] <- 'Not categorised'
Alldata_OB$Materials[!is.na(Alldata_OB$Materials) & Alldata_OB$Materials_cat == 'Not categorised']

table(Alldata_OB$Materials_cat)

## Preprint
Alldata_OB$Preprint_cat[str_detect(Alldata_OB$Preprint, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
Alldata_OB$Preprint_cat[str_detect(Alldata_OB$Preprint, "PEER REVIEW")] <- 'Lack of peer review'

Alldata_OB$Preprint_cat[!is.na(Alldata_OB$Preprint) & is.na(Alldata_OB$Preprint_cat)] <- 'Not categorised'
Alldata_OB$Preprint[!is.na(Alldata_OB$Preprint) & Alldata_OB$Preprint_cat == 'Not categorised']

table(Alldata_OB$Preprint_cat)

## Preregistration
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "PILOT")] <- 'Lack of funding for pilot studies'
Alldata_OB$Prereg_cat[str_detect(Alldata_OB$Prereg, "DISCIPLINE")] <- 'Not applicable to all disciplines'

Alldata_OB$Prereg_cat[!is.na(Alldata_OB$Prereg) & is.na(Alldata_OB$Prereg_cat)] <- 'Not categorised'
Alldata_OB$Prereg[!is.na(Alldata_OB$Prereg) & Alldata_OB$Prereg_cat == 'Not categorised']

table(Alldata_OB$Prereg_cat)


## Registered Report
Alldata_OB$RegRep_cat[str_detect(Alldata_OB$RegRep, "MESSY")] <- 'Impedes flexibility in protocols'

Alldata_OB$RegRep_cat[!is.na(Alldata_OB$RegRep) & is.na(Alldata_OB$RegRep_cat)] <- 'Not categorised'
Alldata_OB$RegRep[!is.na(Alldata_OB$RegRep) & Alldata_OB$RegRep_cat == 'Not categorised']

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
  which(str_detect(Alldata_WD$Data, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Code, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Materials, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Preprint, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$Prereg, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(Alldata_WD$RegRep, "AS FOR|AS IN|SAME AS|\\^")))),]

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

rm(Cells_to_fillup_manually)

## categorise downsides
Alldata_WD$OA_cat <- NA
Alldata_WD$Data_cat <- NA
Alldata_WD$Data_cat2 <- NA
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
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANC*|PAY|CHARGES|POORER")] <- 'Financial cost' # including inequalities of access to publishing between institutions
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "JOURNAL INCOME|PRODUCTION")] <- 'Loss of journal income' # need to find other means of journal production'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "QUALITY|RIGOR*")] <- 'Lowers quality' # reduce quality of peer review if journal paid
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "OPTIONS|LIMITS THE JOURNALS")] <- 'Fewer (prestigious) journal options'
Alldata_WD$OA_cat[str_detect(Alldata_WD$OA, "OPINION")] <- 'Ethical, safety, or security concerns'

Alldata_WD$OA_cat[!is.na(Alldata_WD$OA) & is.na(Alldata_WD$OA_cat)] <- 'Not categorised'
Alldata_WD$OA[!is.na(Alldata_WD$OA) & Alldata_WD$OA_cat == 'Not categorised']

table(Alldata_WD$OA_cat)

## Data
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN|NOT WITHIN THE SCOPE")] <- 'Time investment' # more work, not valued for career, significant burden for qualitative researchers
Alldata_WD$Data_cat2[str_detect(Alldata_WD$Data, "CONTINUITY")] <- 'Challenges around continuity of ownership (e.g. for longitudinal dataset)'
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "ANONYM*|SENSITIVE DATA|PRIVA*|PARTICIPANT DATA|DATA PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|TRICKY|DPA")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "LICENCES|LICENSES|FORMATS")] <- 'Proprietary file format'
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "HARMFUL|MALICIOUS|MISUSE|MALIGN")] <- 'No control over validity of reuse, misrepresentation, misuse'
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "PIPPED|STEALING|SCOOPED")] <- 'Fear of scooping'
Alldata_WD$Data_cat2[str_detect(Alldata_WD$Data, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|CREDIT|COPYRIGHT*|PROTECTION OF IP")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation'
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "FIRST TO PUBLISH")] <- 'Lowers quality' # by increasing \'first to publish\' pressure' 
Alldata_WD$Data_cat[str_detect(Alldata_WD$Data, "NO NORM FOR CITATION")] <- 'No norm for citation'

Alldata_WD$Data_cat[!is.na(Alldata_WD$Data) & is.na(Alldata_WD$Data_cat)] <- 'Not categorised'
Alldata_WD$Data[!is.na(Alldata_WD$Data) & Alldata_WD$Data_cat == 'Not categorised']
Alldata_WD$Data_cat[!is.na(Alldata_WD$Data_cat2) & Alldata_WD$Data_cat == 'Not categorised'] <-  Alldata_WD$Data_cat2[!is.na(Alldata_WD$Data_cat2) & Alldata_WD$Data_cat == 'Not categorised']
Alldata_WD$Data_cat2[!is.na(Alldata_WD$Data_cat2) & Alldata_WD$Data_cat == Alldata_WD$Data_cat2] <- NA
Alldata_WD$Data[!is.na(Alldata_WD$Data) & Alldata_WD$Data_cat == 'Not categorised']

table(c(Alldata_WD$Data_cat, Alldata_WD$Data_cat2))
#View(Alldata_WD[!is.na(Alldata_WD$Data),c('Data', 'Data_cat','Data_cat2')])


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


# OB = Other Barriers
# WD = What Downsides


  # source("Rscripts/FormatData.R")


# pgrdata_OB -----
pgrdata_OB <- prepare_freetext_subdataset(pgrdata, "^OtherBarriers_")

## Nb of responses
pgrdata_OB %>% summarise(across (everything(), ~sum(!is.na(.))))

## check if respondents wrote something like same as previous answer.....
pgrdata_OB[unique(c(
  which(str_detect(pgrdata_OB$Data, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_OB$Code, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_OB$Materials, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_OB$Preprint, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_OB$Prereg, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_OB$RegRep, "AS FOR|AS IN|SAME AS|\\^")))),]


## categorise barriers
pgrdata_OB$OA_cat <- NA
pgrdata_OB$Data_cat <- NA
pgrdata_OB$Code_cat <- NA
pgrdata_OB$Materials_cat <- NA
pgrdata_OB$Preprint_cat <- NA
pgrdata_OB$Prereg_cat <- NA
pgrdata_OB$RegRep_cat <- NA

### OA
pgrdata_OB$OA[!is.na(pgrdata_OB$OA)]
pgrdata_OB$OA_cat[str_detect(pgrdata_OB$OA, "INDUSTRY")] <- 'Resource not owned'
pgrdata_OB$OA_cat[str_detect(pgrdata_OB$OA, c("EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANCIAL|PAY|CHARGES"))] <- 'Financial cost'
pgrdata_OB$OA_cat[str_detect(pgrdata_OB$OA, "JOURNAL QUALITY")] <- 'Lower quality'

pgrdata_OB$OA_cat[!is.na(pgrdata_OB$OA) & is.na(pgrdata_OB$OA_cat)] <- 'Not categorised'
pgrdata_OB$OA[!is.na(pgrdata_OB$OA_cat) & pgrdata_OB$OA_cat == 'Not categorised']

table(pgrdata_OB$OA_cat)

## Data
pgrdata_OB$Data_cat[str_detect(pgrdata_OB$Data, "MANAGING DATA")] <- 'Difficult resource management and lack of metadata standards'
pgrdata_OB$Data_cat[str_detect(pgrdata_OB$Data, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA")] <- 'Ethical concerns'
pgrdata_OB$Data_cat[str_detect(pgrdata_OB$Data, "AUTHORITY|INDUSTRY|COMMERCIAL*")] <- 'Resource not owned'
pgrdata_OB$Data_cat[str_detect(pgrdata_OB$Data, "OEUVERS")] <- 'Resource not always digital'

pgrdata_OB$Data_cat[!is.na(pgrdata_OB$Data) & is.na(pgrdata_OB$Data_cat)] <- 'Not categorised'
pgrdata_OB$Data[!is.na(pgrdata_OB$Data_cat) & pgrdata_OB$Data_cat == 'Not categorised']

table(pgrdata_OB$Data_cat)

## Code
pgrdata_OB$Code_cat[str_detect(pgrdata_OB$Code, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
pgrdata_OB$Code_cat[str_detect(pgrdata_OB$Code, "TIME|LOT OF WORK")] <- 'Time investment'

pgrdata_OB$Code_cat[!is.na(pgrdata_OB$Code) & is.na(pgrdata_OB$Code_cat)] <- 'Not categorised'
pgrdata_OB$Code[!is.na(pgrdata_OB$Code) & pgrdata_OB$Code_cat == 'Not categorised']

table(pgrdata_OB$Code_cat)

## Materials
pgrdata_OB$Materials_cat[str_detect(pgrdata_OB$Materials, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
pgrdata_OB$Materials_cat[str_detect(pgrdata_OB$Materials, "MANAGING DATA")] <- 'Difficult resource management and lack of metadata standards'
pgrdata_OB$Materials_cat[str_detect(pgrdata_OB$Materials, "LIBRARIES")] <- 'Resource not always digital'

pgrdata_OB$Materials_cat[!is.na(pgrdata_OB$Materials) & is.na(pgrdata_OB$Materials_cat)] <- 'Not categorised'
pgrdata_OB$Materials[!is.na(pgrdata_OB$Materials) & pgrdata_OB$Materials_cat == 'Not categorised']

table(pgrdata_OB$Materials_cat)

## Preprint
pgrdata_OB$Preprint_cat[str_detect(pgrdata_OB$Preprint, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
pgrdata_OB$Preprint_cat[str_detect(pgrdata_OB$Preprint, "PEER REVIEW")] <- 'Lack of peer review'

pgrdata_OB$Preprint_cat[!is.na(pgrdata_OB$Preprint) & is.na(pgrdata_OB$Preprint_cat)] <- 'Not categorised'
pgrdata_OB$Preprint[!is.na(pgrdata_OB$Preprint) & pgrdata_OB$Preprint_cat == 'Not categorised']

table(pgrdata_OB$Preprint_cat)

## Preregistration
pgrdata_OB$Prereg_cat[str_detect(pgrdata_OB$Prereg, "PRIOR TO PUBLICATION")] <- 'Fear of scooping'
pgrdata_OB$Prereg_cat[str_detect(pgrdata_OB$Prereg, "PILOT")] <- 'Lack of funding for pilot studies'
pgrdata_OB$Prereg_cat[str_detect(pgrdata_OB$Prereg, "DISCIPLINE")] <- 'Not applicable to all disciplines'

pgrdata_OB$Prereg_cat[!is.na(pgrdata_OB$Prereg) & is.na(pgrdata_OB$Prereg_cat)] <- 'Not categorised'
pgrdata_OB$Prereg[!is.na(pgrdata_OB$Prereg) & pgrdata_OB$Prereg_cat == 'Not categorised']

table(pgrdata_OB$Prereg_cat)


## Registered Report
pgrdata_OB$RegRep_cat[str_detect(pgrdata_OB$RegRep, "MESSY")] <- 'Impedes flexibility in protocols'

pgrdata_OB$RegRep_cat[!is.na(pgrdata_OB$RegRep) & is.na(pgrdata_OB$RegRep_cat)] <- 'Not categorised'
pgrdata_OB$RegRep[!is.na(pgrdata_OB$RegRep) & pgrdata_OB$RegRep_cat == 'Not categorised']

table(pgrdata_OB$RegRep_cat)

  
# staffdata_OB -----
staffdata_OB <- prepare_freetext_subdataset(staffdata, "^OtherBarriers_")

## Nb of responses
staffdata_OB %>% summarise(across (everything(), ~sum(!is.na(.))))

## check if respondents wrote something like same as previous answer.....
staffdata_OB[unique(c(
  which(str_detect(staffdata_OB$Data, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(staffdata_OB$Code, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(staffdata_OB$Materials, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(staffdata_OB$Preprint, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(staffdata_OB$Prereg, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(staffdata_OB$RegRep, "AS FOR|AS IN|SAME AS|\\^")))),]

## categorise barriers
staffdata_OB$OA_cat <- NA
staffdata_OB$Data_cat <- NA
staffdata_OB$Code_cat <- NA
staffdata_OB$Materials_cat <- NA
staffdata_OB$Preprint_cat <- NA
staffdata_OB$Prereg_cat <- NA
staffdata_OB$RegRep_cat <- NA

## OA
staffdata_OB$OA_cat[str_detect(staffdata_OB$OA, c("EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANC*|PAY|CHARGES"))] <- 'Financial cost'

staffdata_OB$OA_cat[!is.na(staffdata_OB$OA) & is.na(staffdata_OB$OA_cat)] <- 'Not categorised'
staffdata_OB$OA[!is.na(staffdata_OB$OA_cat) & staffdata_OB$OA_cat == 'Not categorised']

table(staffdata_OB$OA_cat)

## Data
staffdata_OB$Data[!is.na(staffdata_OB$Data)]
staffdata_OB$Data_cat[str_detect(staffdata_OB$Data, "AUTHORITY|INDUSTRY|COMMERCIAL*")] <- 'Resource not owned'

staffdata_OB$Data_cat[!is.na(staffdata_OB$Data) & is.na(staffdata_OB$Data_cat)] <- 'Not categorised'
staffdata_OB$Data[!is.na(staffdata_OB$Data_cat) & staffdata_OB$Data_cat == 'Not categorised']

table(staffdata_OB$Data_cat)

## Code
staffdata_OB$Code_cat[str_detect(staffdata_OB$Code, "TIME|LOT OF WORK")] <- 'Time investment'

staffdata_OB$Code_cat[!is.na(staffdata_OB$Code) & is.na(staffdata_OB$Code_cat)] <- 'Not categorised'
staffdata_OB$Code[!is.na(staffdata_OB$Code_cat) & staffdata_OB$Code_cat == 'Not categorised']

table(staffdata_OB$Code_cat)

## Materials
staffdata_OB$Materials

# staffdata_OB$Materials_cat[!is.na(staffdata_OB$Materials) & is.na(staffdata_OB$Materials_cat)] <- 'Not categorised'
# staffdata_OB$Materials[!is.na(staffdata_OB$Materials_cat) & staffdata_OB$Materials_cat == 'Not categorised']
# 
# table(staffdata_OB$Materials_cat)

## Preprint
staffdata_OB$Preprint

# staffdata_OB$Preprint_cat[!is.na(staffdata_OB$Preprint) & is.na(staffdata_OB$Preprint_cat)] <- 'Not categorised'
# staffdata_OB$Preprint[!is.na(staffdata_OB$Preprint_cat) & staffdata_OB$Preprint_cat == 'Not categorised']
# 
# table(staffdata_OB$Preprint_cat)


## Prereg
staffdata_OB$Prereg[!is.na(staffdata_OB$Prereg)]

staffdata_OB$Prereg_cat[!is.na(staffdata_OB$Prereg) & is.na(staffdata_OB$Prereg_cat)] <- 'Not categorised'
staffdata_OB$Prereg[!is.na(staffdata_OB$Prereg_cat) & staffdata_OB$Prereg_cat == 'Not categorised']

table(staffdata_OB$Prereg_cat)


## RegRep
staffdata_OB$RegRep

# staffdata_OB$RegRep_cat[!is.na(staffdata_OB$RegRep) & is.na(staffdata_OB$RegRep_cat)] <- 'Not categorised'
# staffdata_OB$RegRep[!is.na(staffdata_OB$RegRep_cat) & staffdata_OB$RegRep_cat == 'Not categorised']
# 
# table(staffdata_OB$RegRep_cat)


# pgrdata_WD -----
pgrdata_WD <- prepare_freetext_subdataset(pgrdata, "^WhatDownsides_")

## Nb of responses
pgrdata_WD %>% summarise(across (everything(), ~sum(!is.na(.))))

## check if respondents wrote something like 'same as previous answer'.....
Cells_to_fillup_manually <- pgrdata_WD[unique(c(
  which(str_detect(pgrdata_WD$Data, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_WD$Code, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_WD$Materials, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_WD$Preprint, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_WD$Prereg, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(pgrdata_WD$RegRep, "AS FOR|AS IN|SAME AS|\\^")))),]

### line 6
pgrdata_WD$Data[!is.na(pgrdata_WD$Data) & pgrdata_WD$Data == 'SAME AS OPEN ACCESS PUBLICATION'] <- pgrdata_WD$OA[!is.na(pgrdata_WD$Data) & pgrdata_WD$Data == 'SAME AS OPEN ACCESS PUBLICATION']
pgrdata_WD$Materials[!is.na(pgrdata_WD$Materials) & pgrdata_WD$Materials == 'SAME AS OPEN ACCESS PUBLICATION'] <- pgrdata_WD$OA[!is.na(pgrdata_WD$Materials) & pgrdata_WD$Materials == 'SAME AS OPEN ACCESS PUBLICATION']

### line 9
pgrdata_WD$Data[!is.na(pgrdata_WD$Data) & pgrdata_WD$Data == '^'] <- pgrdata_WD$OA[!is.na(pgrdata_WD$Data) & pgrdata_WD$Data == '^']
pgrdata_WD$Code[!is.na(pgrdata_WD$Code) & pgrdata_WD$Code == '^'] <- pgrdata_WD$OA[!is.na(pgrdata_WD$Code) & pgrdata_WD$Code == '^']
pgrdata_WD$Materials[!is.na(pgrdata_WD$Materials) & pgrdata_WD$Materials == '^'] <- pgrdata_WD$OA[!is.na(pgrdata_WD$Materials) & pgrdata_WD$Materials == '^']

### line 52
pgrdata_WD$Materials[!is.na(pgrdata_WD$Materials) & pgrdata_WD$Materials == 'AS FOR DATA. '] <- pgrdata_WD$Data[!is.na(pgrdata_WD$Materials) & pgrdata_WD$Materials == 'AS FOR DATA. ']

### line 53
pgrdata_WD$RegRep[!is.na(pgrdata_WD$RegRep) & pgrdata_WD$RegRep == 'SAME AS FOR PREREGISTRATION.'] <- pgrdata_WD$Prereg[!is.na(pgrdata_WD$RegRep) & pgrdata_WD$RegRep == 'SAME AS FOR PREREGISTRATION.']

rm(Cells_to_fillup_manually)

## categorise downsides
pgrdata_WD$OA_cat <- NA
pgrdata_WD$Data_cat <- NA
pgrdata_WD$Data_cat2 <- NA
pgrdata_WD$Code_cat <- NA
pgrdata_WD$Materials_cat <- NA
pgrdata_WD$Preprint_cat <- NA
pgrdata_WD$Prereg_cat <- NA
pgrdata_WD$Prereg_cat2 <- NA
pgrdata_WD$RegRep_cat <- NA
pgrdata_WD$RegRep_cat2 <- NA
pgrdata_WD$RegRep_cat3 <- NA

### OA
pgrdata_WD$OA_cat[str_detect(pgrdata_WD$OA, "HARM*|INAPPROPRIATE")] <- 'No control over validity of reuse, misrepresentation, misuse'
pgrdata_WD$OA_cat[str_detect(pgrdata_WD$OA, "EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANCIAL|PAY|CHARGES")] <- 'Financial cost' # including inequalities of access to publishing between institutions
pgrdata_WD$OA_cat[str_detect(pgrdata_WD$OA, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation
pgrdata_WD$OA_cat[str_detect(pgrdata_WD$OA, "JOURNAL INCOME|PRODUCTION")] <- 'Loss of journal income' # need to find other means of journal production'
pgrdata_WD$OA_cat[str_detect(pgrdata_WD$OA, "QUALITY|RIGOR*")] <- 'Lowers quality' # reduce quality of peer review if journal paid
pgrdata_WD$OA_cat[str_detect(pgrdata_WD$OA, "OPTIONS|LIMITS THE JOURNALS")] <- 'Fewer (prestigious) journal options'
pgrdata_WD$OA_cat[str_detect(pgrdata_WD$OA, "OPINION")] <- 'Ethical, safety, or security concerns'

pgrdata_WD$OA_cat[!is.na(pgrdata_WD$OA) & is.na(pgrdata_WD$OA_cat)] <- 'Not categorised'
pgrdata_WD$OA[!is.na(pgrdata_WD$OA) & pgrdata_WD$OA_cat == 'Not categorised']

table(pgrdata_WD$OA_cat)
  #OA_cat <-  pgrdata_WD[!is.na(pgrdata_WD$OA),c('OA','OA_cat')]

## Data
pgrdata_WD$Data_cat[str_detect(pgrdata_WD$Data, "PIPPED|STEALING|SCOOPED")] <- 'Fear of scooping'
pgrdata_WD$Data_cat[str_detect(pgrdata_WD$Data, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation'
pgrdata_WD$Data_cat[str_detect(pgrdata_WD$Data, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|TRICKY")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
pgrdata_WD$Data_cat[str_detect(pgrdata_WD$Data, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN")] <- 'Time investment' # more work, not valued for career, significant burden for qualitative researchers
pgrdata_WD$Data_cat2[str_detect(pgrdata_WD$Data, "CONTINUITY")] <- 'Challenges around continuity of ownership (e.g. for longitudinal dataset)'
pgrdata_WD$Data_cat[str_detect(pgrdata_WD$Data, "FIRST TO PUBLISH")] <- 'Lowers quality' # by increasing \'first to publish\' pressure' 
pgrdata_WD$Data_cat[str_detect(pgrdata_WD$Data, "HARMFUL|MALICIOUS|MISUSE")] <- 'No control over validity of reuse, misrepresentation, misuse'

pgrdata_WD$Data_cat[!is.na(pgrdata_WD$Data) & is.na(pgrdata_WD$Data_cat)] <- 'Not categorised'
pgrdata_WD$Data[!is.na(pgrdata_WD$Data) & pgrdata_WD$Data_cat == 'Not categorised']

table(c(pgrdata_WD$Data_cat, pgrdata_WD$Data_cat2))
  #Data_cat <-  pgrdata_WD[!is.na(pgrdata_WD$Data),c('Data','Data_cat','Data_cat2')]  #not catd: "Academics are too protective over their work"


## Code
pgrdata_WD$Code_cat[str_detect(pgrdata_WD$Code, "POOR QUALITY|REVIEWED AND TESTED")] <- 'Lowers quality' # by propagating unreviewed and untested code
pgrdata_WD$Code_cat[str_detect(pgrdata_WD$Code, "LONG TIME")] <- 'Time investment'
pgrdata_WD$Code_cat[str_detect(pgrdata_WD$Code, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED")] <- 'Intellectual property concerns' # (including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
pgrdata_WD$Code_cat[str_detect(pgrdata_WD$Code, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
pgrdata_WD$Code_cat[str_detect(pgrdata_WD$Code, "FIRST TO PUBLISH")] <- 'Lowers quality' # by increasing \'first to publish\' pressure
pgrdata_WD$Code_cat[str_detect(pgrdata_WD$Code, "HARMFUL|MALICIOUS|MISUSE|INAPPRORPIATE|NOT UNDERSTAND|NUANCE")] <- 'No control over validity of reuse, misrepresentation, misuse'

pgrdata_WD$Code_cat[!is.na(pgrdata_WD$Code) & is.na(pgrdata_WD$Code_cat)] <- 'Not categorised'
pgrdata_WD$Code[!is.na(pgrdata_WD$Code) & pgrdata_WD$Code_cat == 'Not categorised']

table(pgrdata_WD$Code_cat)
  #Code_cat <-  pgrdata_WD[!is.na(pgrdata_WD$Code),c('Code','Code_cat')]  

## Materials
pgrdata_WD$Materials_cat[str_detect(pgrdata_WD$Materials, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|RADIOACTIVE")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
pgrdata_WD$Materials_cat[str_detect(pgrdata_WD$Materials, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED")] <- 'Intellectual property concerns' # (including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
pgrdata_WD$Materials_cat[str_detect(pgrdata_WD$Materials, "FIRST TO PUBLISH")] <- 'Lowers quality'# by increasing \'first to publish\' pressure
pgrdata_WD$Materials_cat[str_detect(pgrdata_WD$Materials, "PIPPED|STEALING|SCOOPED|PUBLISH PAPERS FAST")] <- 'Fear of scooping' # leading to loss of career prospect
pgrdata_WD$Materials_cat[str_detect(pgrdata_WD$Materials, "HARMFUL|MALICIOUS|MISUSE|INAPPRORPIATE|NOT UNDERSTAND|NUANCE")] <- 'No control over validity of reuse, misrepresentation, misuse'
pgrdata_WD$Materials_cat[str_detect(pgrdata_WD$Materials, "RETURNED")] <- 'Resource not owned' # (e.g. archelogical artifacts)
pgrdata_WD$Materials_cat[str_detect(pgrdata_WD$Materials, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN")] <- 'Time investment' # more work, not valued for career, significant burden for qualitative researchers
pgrdata_WD$Materials_cat[str_detect(pgrdata_WD$Materials, "PLATFORMS")] <- 'Unusable if not from open source platforms' # 

pgrdata_WD$Materials_cat[!is.na(pgrdata_WD$Materials) & is.na(pgrdata_WD$Materials_cat)] <- 'Not categorised'
pgrdata_WD$Materials[!is.na(pgrdata_WD$Materials) & pgrdata_WD$Materials_cat == 'Not categorised']

table(pgrdata_WD$Materials_cat) 
  #Materials_cat <-  pgrdata_WD[!is.na(pgrdata_WD$Materials),c('Materials','Materials_cat')]  

## Preprint
pgrdata_WD$Preprint_cat[str_detect(pgrdata_WD$Preprint, "TIME REQUIRED")] <- 'Time investment'
pgrdata_WD$Preprint_cat[str_detect(pgrdata_WD$Preprint, "PUBLICATION OF FULL PAPER|PREVENT PUBLICATION|TOP JOURNALS")] <- 'Prevents or complicates formal publishing'
pgrdata_WD$Preprint_cat[str_detect(pgrdata_WD$Preprint, "PEER REVIEW*|UNRELIABLE|ISSUES|PEER-REVIEW*|ERRO*|POOR QUALITY|SCHOLARS")] <- 'Misleading due to lack of peer review' # (e.g. public, media, other researchers (for new research or as citation)) and requires to revisit the final version (which few will do)'
pgrdata_WD$Preprint_cat[str_detect(pgrdata_WD$Preprint, "BLIND PEER REVIEW*")] <- 'Interferes with blind peer reviewing' # to overwrite the simple 'peer review' above
pgrdata_WD$Preprint_cat2[str_detect(pgrdata_WD$Preprint, "FIRST TO PUBLISH")] <- 'Lowers quality' # by increasing \'first to publish\' pressure' 
pgrdata_WD$Preprint_cat[str_detect(pgrdata_WD$Preprint, "FASTER")] <- 'Fear of scooping' 

pgrdata_WD$Preprint_cat[!is.na(pgrdata_WD$Preprint) & is.na(pgrdata_WD$Preprint_cat)] <- 'Not categorised'
pgrdata_WD$Preprint[!is.na(pgrdata_WD$Preprint) & pgrdata_WD$Preprint_cat == 'Not categorised']

table(pgrdata_WD$Preprint_cat)
  #Preprint_cat <-  pgrdata_WD[!is.na(pgrdata_WD$Preprint),c('Preprint','Preprint_cat')] 

## Preregistration
pgrdata_WD$Prereg_cat[str_detect(pgrdata_WD$Prereg, "PIPPED|STEAL*|SCOOPED|PUBLISH BEFORE YOU|COMPETITIVE")] <- 'Fear of scooping' 
pgrdata_WD$Prereg_cat[str_detect(pgrdata_WD$Prereg, "MORE TIME|SLOW DOWN|TIME FOR RESEARCH")] <- 'Time investment' 
pgrdata_WD$Prereg_cat[str_detect(pgrdata_WD$Prereg, "NO EXPERIMENTATION|NOT RELEVANT")] <- 'Not relevant for all fields' # (e.g. theoretical, mathematical research where biases are not present or in the humanities that do not follow a scientific process)' 
pgrdata_WD$Prereg_cat[str_detect(pgrdata_WD$Prereg, "EXPLORATORY|BLUE SKIES")] <- 'Impedes exploratory research' 
pgrdata_WD$Prereg_cat[str_detect(pgrdata_WD$Prereg, "NULL FINDINGS")] <- 'Needs corresponding increase in respect for null findings' 
pgrdata_WD$Prereg_cat2[str_detect(pgrdata_WD$Prereg, "EVOLVING|HYPOTHESES CHANGE|WIGGLE ROOM|ADAPT TO UNFORESEEN")] <- 'Impedes flexibility in protocols'

pgrdata_WD$Prereg_cat[!is.na(pgrdata_WD$Prereg) & is.na(pgrdata_WD$Prereg_cat)] <- 'Not categorised'
pgrdata_WD$Prereg[!is.na(pgrdata_WD$Prereg) & pgrdata_WD$Prereg_cat == 'Not categorised']
pgrdata_WD$Prereg_cat[!is.na(pgrdata_WD$Prereg_cat2) & pgrdata_WD$Prereg_cat == 'Not categorised'] <- pgrdata_WD$Prereg_cat2[!is.na(pgrdata_WD$Prereg_cat2) & pgrdata_WD$Prereg_cat == 'Not categorised']
pgrdata_WD$Prereg_cat2[!is.na(pgrdata_WD$Prereg_cat2) & pgrdata_WD$Prereg_cat == pgrdata_WD$Prereg_cat2] <- NA

pgrdata_WD$Prereg[!is.na(pgrdata_WD$Prereg) & pgrdata_WD$Prereg_cat == 'Not categorised']
pgrdata_WD$Prereg[!is.na(pgrdata_WD$Prereg) & pgrdata_WD$Prereg_cat2 == 'Not categorised']

table(c(pgrdata_WD$Prereg_cat,pgrdata_WD$Prereg_cat2))
  #Prereg_cat <-  pgrdata_WD[!is.na(pgrdata_WD$Prereg),c('Prereg','Prereg_cat', 'Prereg_cat2')]  
  

## Registered Report
pgrdata_WD$RegRep_cat[str_detect(pgrdata_WD$RegRep, "PIPPED|STEAL*|SCOOPED|PUBLISH BEFORE YOU|COMPETITIVE|POACHING|COPY METHOD")] <- 'Fear of scooping' 
pgrdata_WD$RegRep_cat2[str_detect(pgrdata_WD$RegRep, "EVOLVING|HYPOTHESES CHANGE|WIGGLE ROOM|UPDATE PROTOCOL|ADAPT TO UNFORESEEN")] <- 'Impedes flexibility in protocols' # 'the understanding of data (e.g. longitudinal data) or the developement of hypothesis for an experiment are evolving, and this process is valuable, it should not be forced into a preregistration'
pgrdata_WD$RegRep_cat3[str_detect(pgrdata_WD$RegRep, "MORE TIME|SLOW DOWN|TIME FOR RESEARCH|SLOWS")] <- 'Time investment' 
pgrdata_WD$RegRep_cat[str_detect(pgrdata_WD$RegRep, "TIMESCALE")] <- 'Challenging timescale for ECR under short term contracts' 
pgrdata_WD$RegRep_cat[str_detect(pgrdata_WD$RegRep, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|ACKNOWLEDGED")] <- 'Intellectual property concerns' #including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation)'
pgrdata_WD$RegRep_cat[str_detect(pgrdata_WD$RegRep, "EXPLORATORY|BLUE SKIES")] <- 'Impedes exploratory research' 

pgrdata_WD$RegRep_cat[!is.na(pgrdata_WD$RegRep) & is.na(pgrdata_WD$RegRep_cat)] <- 'Not categorised'
pgrdata_WD$RegRep[!is.na(pgrdata_WD$RegRep) & pgrdata_WD$RegRep_cat == 'Not categorised']
pgrdata_WD$RegRep_cat[!is.na(pgrdata_WD$RegRep_cat2) & pgrdata_WD$RegRep_cat == 'Not categorised'] <- pgrdata_WD$RegRep_cat2[!is.na(pgrdata_WD$RegRep_cat2) & pgrdata_WD$RegRep_cat == 'Not categorised']
pgrdata_WD$RegRep_cat2[!is.na(pgrdata_WD$RegRep_cat2) & pgrdata_WD$RegRep_cat == pgrdata_WD$RegRep_cat2] <- NA
pgrdata_WD$RegRep_cat[!is.na(pgrdata_WD$RegRep_cat3) & pgrdata_WD$RegRep_cat == 'Not categorised'] <- pgrdata_WD$RegRep_cat3[!is.na(pgrdata_WD$RegRep_cat3) & pgrdata_WD$RegRep_cat == 'Not categorised']
pgrdata_WD$RegRep_cat3[!is.na(pgrdata_WD$RegRep_cat3) & pgrdata_WD$RegRep_cat == pgrdata_WD$RegRep_cat3] <- NA

pgrdata_WD$RegRep[!is.na(pgrdata_WD$RegRep) & pgrdata_WD$RegRep_cat == 'Not categorised']
pgrdata_WD$RegRep[!is.na(pgrdata_WD$RegRep) & pgrdata_WD$RegRep_cat2 == 'Not categorised']
pgrdata_WD$RegRep[!is.na(pgrdata_WD$RegRep) & pgrdata_WD$RegRep_cat3 == 'Not categorised']

table(c(pgrdata_WD$RegRep_cat, pgrdata_WD$RegRep_cat2, pgrdata_WD$RegRep_cat3))
  #RegRep_cat <-  pgrdata_WD[!is.na(pgrdata_WD$RegRep),c('RegRep','RegRep_cat', 'RegRep_cat2','RegRep_cat3')]  #not catd: "Academics are too protective over their work"


# staffdata_WD -----
staffdata_WD <- prepare_freetext_subdataset(staffdata, "^WhatDownsides_")

## Nb of responses
staffdata_WD %>% summarise(across (everything(), ~sum(!is.na(.))))

## check if respondents wrote something like same as previous answer.....
staffdata_WD[unique(c(
  which(str_detect(staffdata_WD$Data, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(staffdata_WD$Code, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(staffdata_WD$Materials, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(staffdata_WD$Preprint, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(staffdata_WD$Prereg, "AS FOR|AS IN|SAME AS|\\^")),
  which(str_detect(staffdata_WD$RegRep, "AS FOR|AS IN|SAME AS|\\^")))),]

## categorise barriers
staffdata_WD$OA_cat <- NA
staffdata_WD$Data_cat <- NA
staffdata_WD$Code_cat <- NA
staffdata_WD$Code_cat2 <- NA
staffdata_WD$Materials_cat <- NA
staffdata_WD$Preprint_cat <- NA
staffdata_WD$Prereg_cat <- NA
staffdata_WD$RegRep_cat <- NA

## OA
staffdata_WD$OA_cat[str_detect(staffdata_WD$OA, c("EXPENSIVE|FEE*|COST*|MONEY|FUND*|FINANCIAL|PAY|CHARGES|POORER"))] <- 'Financial cost'
staffdata_WD$OA_cat[str_detect(staffdata_WD$OA, "OPTIONS|LIMITS THE JOURNALS")] <- 'Fewer (prestigious) journal options'

staffdata_WD$OA_cat[!is.na(staffdata_WD$OA) & is.na(staffdata_WD$OA_cat)] <- 'Not categorised'
staffdata_WD$OA[!is.na(staffdata_WD$OA) & staffdata_WD$OA_cat == 'Not categorised']

table(staffdata_WD$OA_cat)

## Data
staffdata_WD$Data[!is.na(staffdata_WD$Data)]
staffdata_WD$Data_cat[str_detect(staffdata_WD$Data, "NO NORM FOR CITATION")] <- 'No norm for citation'
staffdata_WD$Data_cat[str_detect(staffdata_WD$Data, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN|NOT WITHIN THE SCOPE")] <- 'Time investment' # more work, not valued for career, significant burden for qualitative researchers
staffdata_WD$Data_cat[str_detect(staffdata_WD$Data, "ANONYM*|SENSITIV*|PRIVA*|PARTICIPANT DATA|PROTECTION|SECURITY|IDENTIF*|ETHIC*|SAFETY|TRICKY|DPA")] <- 'Ethical, safety, or security concerns' # human participants, archeological site, endengered animal/plant species, military information
staffdata_WD$Data_cat[str_detect(staffdata_WD$Data, "LICENCES|LICENSES|FORMATS")] <- 'Proprietary file format'
staffdata_WD$Data_cat[str_detect(staffdata_WD$Data, "HARMFUL|MALICIOUS|MISUSE|MALIGN")] <- 'No control over validity of reuse, misrepresentation, misuse'
staffdata_WD$Data_cat2[str_detect(staffdata_WD$Data, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|PROTECTION OF IP")] <- 'Intellectual property concerns' # including plagiarism, duplication of research, difficulty with navigating copyright, and loss of payment to author or commercialisation'

staffdata_WD$Data_cat[!is.na(staffdata_WD$Data) & is.na(staffdata_WD$Data_cat)] <- 'Not categorised'
staffdata_WD$Data[!is.na(staffdata_WD$Data) & staffdata_WD$Data_cat == 'Not categorised']

table(staffdata_WD$Data_cat)

## Code
staffdata_WD$Code[!is.na(staffdata_WD$Code)]
staffdata_WD$Code_cat[str_detect(staffdata_WD$Code, "MORE WORK|WORKLOAD|OVERHEAD|BURDEN|NOT WITHIN THE SCOPE|SLOW DOWN")] <- 'Time investment'
staffdata_WD$Code_cat[str_detect(staffdata_WD$Code, "REDUCE THE NUMBER OF APPROACHES USED")] <- 'Reduce diversity of approaches'
staffdata_WD$Code_cat2[str_detect(staffdata_WD$Code, "FIND PEOPLE TO REVIEW THE CODE")] <- 'No time or expertise to review code'
staffdata_WD$Code_cat[str_detect(staffdata_WD$Code, "DUPLICATION|PLAGIA*|COMMERC*|PATENT*|APPROVAL|CREDIT|COPYRIGHT*|PROTECTION OF IP")] <- 'Intellectual property concerns' 

staffdata_WD$Code_cat[!is.na(staffdata_WD$Code) & is.na(staffdata_WD$Code_cat)] <- 'Not categorised'
staffdata_WD$Code[!is.na(staffdata_WD$Code) & staffdata_WD$Code_cat == 'Not categorised']
staffdata_WD$Code_cat[!is.na(staffdata_WD$Code_cat2) & staffdata_WD$Code_cat == 'Not categorised'] <- staffdata_WD$Code_cat2[!is.na(staffdata_WD$Code_cat2) & staffdata_WD$Code_cat == 'Not categorised']
staffdata_WD$Code_cat2[!is.na(staffdata_WD$Code_cat2) & staffdata_WD$Code_cat == staffdata_WD$Code_cat2] <- NA

staffdata_WD$Code[!is.na(staffdata_WD$Code) & staffdata_WD$Code_cat == 'Not categorised']
staffdata_WD$Code[!is.na(staffdata_WD$Code) & staffdata_WD$Code_cat2 == 'Not categorised']

table(staffdata_WD$Code_cat)

## Materials
staffdata_WD$Materials[!is.na(staffdata_WD$Materials)]
staffdata_WD$Materials_cat[str_detect(staffdata_WD$Materials, "LICENCES|LICENSES")] <- 'Resource not own'
staffdata_WD$Materials_cat[str_detect(staffdata_WD$Materials, "NATIONAL POLICIES")] <- 'Law against sharing specific material'
staffdata_WD$Materials_cat[str_detect(staffdata_WD$Materials, "FRAGILE")] <- 'Impractical'

staffdata_WD$Materials_cat[!is.na(staffdata_WD$Materials) & is.na(staffdata_WD$Materials_cat)] <- 'Not categorised'
staffdata_WD$Materials[!is.na(staffdata_WD$Materials) & staffdata_WD$Materials_cat == 'Not categorised']

table(staffdata_WD$Materials_cat)

## Preprint
staffdata_WD$Preprint[!is.na(staffdata_WD$Preprint)]
staffdata_WD$Preprint_cat[str_detect(staffdata_WD$Preprint, "ECOLOGICAL COST")] <- 'Ecological cost'
staffdata_WD$Preprint_cat[str_detect(staffdata_WD$Preprint, "PEER-REVIEW")] <- 'No peer-review, reliance on reputation'

staffdata_WD$Preprint_cat[!is.na(staffdata_WD$Preprint) & is.na(staffdata_WD$Preprint_cat)] <- 'Not categorised'
staffdata_WD$Preprint[!is.na(staffdata_WD$Preprint) & staffdata_WD$Preprint_cat == 'Not categorised']

table(staffdata_WD$Preprint_cat)


## Prereg
staffdata_WD$Prereg[!is.na(staffdata_WD$Prereg)]
staffdata_WD$Prereg_cat[str_detect(staffdata_WD$Prereg, "MORE TIME|SLOW DOWN|TIME FOR RESEARCH|TIME IT TAKES")] <- 'Time investment' 
staffdata_WD$Prereg_cat[str_detect(staffdata_WD$Prereg, "EVOLVING|HYPOTHESES CHANGE|WIGGLE ROOM|UPDATE PROTOCOL|ADAPT TO UNFORESEEN|FORTUITOUS AND UNPREDICTABLE")] <- 'Impedes flexibility in protocols' # 

staffdata_WD$Prereg_cat[!is.na(staffdata_WD$Prereg) & is.na(staffdata_WD$Prereg_cat)] <- 'Not categorised'
staffdata_WD$Prereg[!is.na(staffdata_WD$Prereg) & staffdata_WD$Prereg_cat == 'Not categorised']

table(staffdata_WD$Prereg_cat)


## RegRep
staffdata_WD$RegRep[!is.na(staffdata_WD$RegRep)]
staffdata_WD$RegRep_cat[str_detect(staffdata_WD$RegRep, "TIME COST|MORE TIME|SLOW DOWN|TIME FOR RESEARCH")] <- 'Time investment' 
staffdata_WD$RegRep_cat[str_detect(staffdata_WD$RegRep, "EVOLVING|HYPOTHESES CHANGE|WIGGLE ROOM|UPDATE PROTOCOL|ADAPT TO UNFORESEEN|FORTUITOUS AND UNPREDICTABLE")] <- 'Impedes flexibility in protocols' # 

staffdata_WD$RegRep_cat[!is.na(staffdata_WD$RegRep) & is.na(staffdata_WD$RegRep_cat)] <- 'Not categorised'
staffdata_WD$RegRep[!is.na(staffdata_WD$RegRep) & staffdata_WD$RegRep_cat == 'Not categorised']

table(staffdata_WD$RegRep_cat)


# Create list for checking categories -----
a_pgrdata_OB<- create_list_for_checking_cat(pgrdata_OB)
a_staffdata_OB <- create_list_for_checking_cat(staffdata_OB)

a_pgrdata_WD <- create_list_for_checking_cat(pgrdata_WD)
a_staffdata_WD <- create_list_for_checking_cat(staffdata_WD)

## to print list for someone to check recoding categories
list_for_checking_cat <- rbind(a_pgrdata_OB, a_staffdata_OB, a_pgrdata_WD, a_staffdata_WD)

#write.csv(list_for_checking_cat, file='list_for_checking_cat.csv')

# Create pivot tables from list for checking categories
pgrdata_OB_table <- create_pivot_table_from_list_for_checking_cat(a_pgrdata_OB)
staffdata_OB_table <- create_pivot_table_from_list_for_checking_cat(a_staffdata_OB)

pgrdata_WD_table <- create_pivot_table_from_list_for_checking_cat(a_pgrdata_WD)
staffdata_WD_table <- create_pivot_table_from_list_for_checking_cat(a_staffdata_WD)


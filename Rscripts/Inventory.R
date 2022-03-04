
  #source("Rscripts/FormatData.R")

Alldata_Inventory <- data_round12[,grep(pattern="^Inventory", x=colnames(data_round12))] 
Alldata_Inventory <- data.frame(stack(Alldata_Inventory))$values
Alldata_Inventory <- Alldata_Inventory[!is.na(Alldata_Inventory)]
Alldata_Inventory <- data.frame(initiative = toupper(Alldata_Inventory))
Alldata_Inventory$initiative_cat <- NA
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("T THINK|NO IDEA|T KNOW|NOT AWARE|NOT SURE|UNKNOWN|UNSURE|N/A|UNAWARE|NONE I CAN THINK OF"))] <- 'Not categorised'

Alldata_Inventory$initiative[!is.na(Alldata_Inventory$initiative_cat)]
Alldata_Inventory$initiative[is.na(Alldata_Inventory$initiative_cat)]
Alldata_Inventory <- Alldata_Inventory[is.na(Alldata_Inventory$initiative_cat),]

## Nb of responses
nrow(Alldata_Inventory)

Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("REPRODUCIBLE RESEARCH OXFORD|RROX|VERENA|MALIKA|OXFORD BERLIN SUMMER SCHOOL|REPRODUCIBILITEA|OXFORD CODE REVIEW NETWORK|OXFORD FREE OPEN SOURCE SOFTWARE|LINKS ABOVE|SABS R3|SABS:R3|EPSRC SABS R³ CDT|DTC|DTP|ZOOLOGY"))] <- 'RROx, reproT, OxCRN, OxFOSS, SABS R3, DTC Stats Course'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("BODLEIAN|HTTP://OPENACCESS.OX.AC.UK|ISKILLS|LIBRARY|OPEN ACCESS OXFORD|^ORA$"))] <- 'Bodleians courses'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("DATA MANAGEMENT PLAN|IT LEARNING CENTRE|IT SERVICES|COSY"))] <- 'IT and Research Data Team Courses'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("DEPARTMENT|OII WEBINAR|READING GROUPS|HISTORY FACULTY|DIVISION WORKSHOPS|HTTPS://WWW.MPLS.OX.AC.UK/TRAINING|MSD SKILLS TRAINING"))] <- 'Departmental or Divisional Skill Training'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("HTTPS://WWW.WIN.OX.AC.UK|OPEN NEUROIMAGING|OPEN WIN|WELLCOME CENTRE FOR INTEGRATIVE NEUROIMAGING|WIN"))] <- 'WIN'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("TERMS4FAIRSKILLS|FAIRSHARING|FAIR OPEN ACCESS ALLIANCE"))] <- 'Terms 4 FAIR Skills, FAIRsharing, FAIR OA Alliance'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("CLARIN"))] <- 'Common Languages Resources Infrastructure'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("SOFTWARE SUSTAINABILITY INSTITUTE"))] <- 'Software Sustainability Institute'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("EQUATOR"))] <- 'Equator Network'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("HTTPS://WWW.SDS.OX.AC.UK/HOME"))] <- 'Sustainable Digital Scholarship'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("HTTPS://WWW.PREPRINTCLUB.COM/"))] <- 'Immunology preprint club'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("ISICILY"))] <- 'I-Sicily'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("CENTRE FOR ENVIRONMENTAL DATA ANALYSIS"))] <- 'Center for Environmental data analyses'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("LITERARY AND LINGUISTIC DATA SERVICE"))] <- 'Literary and Linguistic Data Service'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("INTERNATIONAL NETWORK FOR DATA ON IMPACT AND GOVERNMENT OUTCOMES"))] <- 'International Network for data on impact and government outcomes'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("OPEN NETWORKING FOUNDATION"))] <- 'Open Networking Foundation'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("OXFORD RESEARCH SOFTWARE ENGINEERING"))] <- 'OxRSE'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("JOURNAL OF THE OXFORD GRADUATE THEOLOGICAL SOCIETY"))] <- 'Journal of the Oxford Graduate Theological Society'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("WEATHERHALL INSTITUTE OF MOLECULAR MEDICINE"))] <- 'WIMM DPhil Course'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("UNIQ+"))] <- 'UNIQ+'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("DIGITAL HUMANITIES+"))] <- 'Digital Humanities Summer School'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("JESUS COLLEGE DIGITAL HUB"))] <- 'Jesus College Digital Hub'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("v"))] <- 'DTC course (to which RROx contributed)'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("R USER GROUP OXFORD"))] <- 'Oxford R user group'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("CGHE"))] <- 'Centre for Global Higher Education'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("GLOBAL HEALTH NETWORK "))] <- 'Global Health Network'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("HDRUK"))] <- 'Health Data Research UK'
Alldata_Inventory$initiative_cat[str_detect(Alldata_Inventory$initiative, 
                                            c("WELLCOME STRATEGY"))] <- 'Wellcome Trust'

Alldata_Inventory$initiative_cat[is.na(Alldata_Inventory$initiative_cat)] <- "Not categorized"
#View(Alldata_Inventory[Alldata_Inventory$initiative_cat == "Not categorized",])


Table_Inventory <- data.frame(table(Alldata_Inventory$initiative_cat[Alldata_Inventory$initiative_cat != "Not categorized"]))
colnames(Table_Inventory) <- c("initiatives", "count")
Table_Inventory %>% arrange(-count)

Quote_Inventory_1 <- data$Inventory1[!is.na(data$Inventory1) & startsWith(data$Inventory1, "This survey seems to imagine")==TRUE]
Quote_Inventory_2 <- data$Inventory1[!is.na(data$Inventory1) & startsWith(data$Inventory1, "All academics and researchers should")==TRUE]




  #source("Rscripts/FormatData.R")

# pgrdata_Inventory
pgrdata_Inventory <- pgrdata[,grep(pattern="^Inventory", x=colnames(pgrdata))]
pgrdata_Inventory <- data.frame(stack(pgrdata_Inventory))$values
pgrdata_Inventory <- pgrdata_Inventory[!is.na(pgrdata_Inventory)]
pgrdata_Inventory <- data.frame(initiative = toupper(pgrdata_Inventory))
pgrdata_Inventory$initiative_cat <- NA
pgrdata_Inventory$initiative_cat[str_detect(pgrdata_Inventory$initiative, 
                                        c("T THINK|NO IDEA|T KNOW|NOT AWARE|NOT SURE"))] <- 'Not categorised'

pgrdata_Inventory$initiative[!is.na(pgrdata_Inventory$initiative_cat)]
pgrdata_Inventory$initiative[is.na(pgrdata_Inventory$initiative_cat)]
pgrdata_Inventory <- pgrdata_Inventory[is.na(pgrdata_Inventory$initiative_cat),]

## Nb of responses
nrow(pgrdata_Inventory)
pgrdata_Inventory <- data.frame(pgrdata_Inventory)

# allstaffdata_Inventory
allstaffdata_Inventory <- allstaffdata[,grep(pattern="^Inventory", x=colnames(allstaffdata))]
allstaffdata_Inventory <- data.frame(stack(allstaffdata_Inventory))$values
allstaffdata_Inventory <- allstaffdata_Inventory[!is.na(allstaffdata_Inventory)]
allstaffdata_Inventory <- data.frame(initiative = toupper(allstaffdata_Inventory))

## Nb of responses
nrow(allstaffdata_Inventory)

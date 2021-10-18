
  #source("Rscripts/FormatData.R")

# pgrdata_Inventory
pgrdata_Inventory <- pgrdata[,grep(pattern="^Inventory", x=colnames(pgrdata))]
pgrdata_Inventory <- data.frame(stack(pgrdata_Inventory))$values
pgrdata_Inventory <- pgrdata_Inventory[!is.na(pgrdata_Inventory)]

## Nb of responses
length(pgrdata_Inventory)


# staffdata_Inventory
staffdata_Inventory <- staffdata[,grep(pattern="^Inventory", x=colnames(staffdata))]
staffdata_Inventory <- data.frame(stack(staffdata_Inventory))$values
staffdata_Inventory <- staffdata_Inventory[!is.na(staffdata_Inventory)]

## Nb of responses
length(staffdata_Inventory)
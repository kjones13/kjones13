saveRDS(kjToDelete, 'C:/Users/kjones/Documents/GitHub/NEON-OS-optimization/phenology/dataCleaning/for2022release/kjToDelete_from00.rds')

library(tidyverse)
library(restR)

df <- get.os.l0.by.query(stack='prod',
                         tab='DP0.10002.001:phe_perindividual_in',
                         #fieldDataSearchStrings = "LTR.2021.TEAK.07",
                         #fieldDataSearchStrings=c("b97ebf06-8e42-4676-a618-a3535c3b08d4"),
                         tag="NEON.PLA.D03.OSBS.06548",
                         #fieldName = 'eventID',
                         #parentNamedLoc = "SRER",
                         #minEndDate = "2019-01-01", 
                         #maxEndDate = "2020-01-01",
                         format_for_L0_editor=TRUE)

unique(df$taxonID)

table(df$taxonID, 
      df$editedDate)

table(df$taxonID, 
      df$transactionDate)

table(df$taxonID, 
      df$clientCreatedDate)

del <- df$fulcrumID[2:8]
del <- df$fulcrumID[df$taxonID!="PELA4"]
del <- df$fulcrumID[df$transactionDate!=max(df$transactionDate)]
del <- df$fulcrumID[df$clientCreatedDate!=max(df$clientCreatedDate)]
del <- df$fulcrumID[df$editedDate!=max(df$editedDate)]
del <- df$fulcrumID[df$fulcrumID!="548b9d7d-1910-48d1-a612-26825ee5a6a5"]


fidsToDelete <- c(fidsToDelete, del)
length(unique(fidsToDelete))
fidsToDelete <- unique(fidsToDelete)

saveRDS(fidsToDelete, 'C:/Users/kjones/Documents/GitHub/NEON-OS-optimization/phenology/dataCleaning/for2022release/fidsToDelete.rds')

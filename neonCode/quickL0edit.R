library(restR)
library(restR2)
library(tidyverse)

l0Box <- 'C:/Users/kjones/Box/L0dataEditing'

newFolder <- 'phe_siDupes_20250218'
dir.create(file.path(l0Box, newFolder), recursive = TRUE)

newSubs <- c('originalL0download', 'editedL0upload', 'editingActivities', 'comparisonResults')

for(i in newSubs){
  dir.create(path=paste(l0Box, newFolder, i, sep='/'), recursive = TRUE)
}

## get L0 data
#ltr dpID="DP0.10033.001"
#phe dpID= "DP0.10002.001"
#vst dpID= "DP0.10098.001"


##restR
df_l0_si <- get.os.l0.by.query(stack='prod',
                         tab='DP0.10002.001:phe_statusintensity_in',
                         fieldDataSearchStrings= "ba6b275a-cb1d-456e-9e0b-a1dd16346b48",
                         #tag="NEON.PLA.D17.SOAP.06675",
                         fieldName = 'fulcrumID',
                         #parentNamedLoc = "SCBI",
                         format_for_L0_editor=TRUE)

##restR2 - l0

df_l0_si <- data.frame()
for(i in unique(df_l0$individualID)){
  temp <- get.os.l0.data(stack = 'prod', 
                        dpID = 'DP0.10002.001',
                        ingestTable = 'phe_statusintensity_in',
                        sampleTag = i)
  df_l0_si <- bind_rows(df_l0_si, temp)
}


# parallel
df_l0 <- par.get.os.l0.data(stack='prod',
                           dpID = 'DP0.10002.001', 
                           ingestTable='phe_statusintensity_in',
                          parentNamedLocation = "GUAN",#c("SCBI", "BLAN"), 
                          #namedLocationName=
                          inclDescendants = "true",
                          startDate = "2024-02-12",
                          endDate = "2024-02-14",
                           #searchString='trapType:ground',
                           format_for_L0_editor=TRUE)

table(df_l0$plotID, df_l0$fulcrumID)

dupes <- df_l0 %>% 
  group_by( individualID) %>% #dayOfYear,, phenophaseName
  filter(n()>1)


write.csv(df_l0_si, paste(l0Box, newFolder, 'originalL0download/df_l0_si_orig.csv', sep='/'), row.names = FALSE)

## DELETE
uuid_only <- select(df_l0_si, uuid=uid)

write.table(uuid_only, paste(l0Box, "/", newFolder, "/editedL0upload/mod_table_delete_uuidOnly.txt", sep=''), 
            sep="\t", row.names = FALSE, col.names="uuid", na='')

## EDIT

df_out <- df

## edit df_out records as needed...

write.table(df_out, paste(l0Box, "/", newFolder, "/editedL0upload/mod_table_edit.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')


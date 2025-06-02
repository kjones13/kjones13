library(tidyverse)
library(restR)

#2023 release
df <- read.csv('C:/Users/kjones/Desktop/forOffline_workingDir/pheOptimization/phePerInd_deleteTheseDups_07Sep2022.csv', stringsAsFactors = F)


l0Box <- 'C:/Users/kjones.ECO/Box/L0dataEditing'

newFolder <- 'phe_perind_2023releaseClean'
dir.create(file.path(l0Box, newFolder), recursive = TRUE)

newSubs <- c('originalL0download', 'editedL0upload', 'editingActivities', 'comparisonResults')

for(i in newSubs){
  dir.create(path=paste(l0Box, newFolder, i, sep='/'), recursive = TRUE)
}


write.csv(df, paste(l0Box, newFolder, 'originalL0download/phe_ind_dupesToDelete.csv', sep='/'), row.names = FALSE)

## DELETE
uuid_only <- select(df, uuid) #=uid)

write.table(uuid_only, paste(l0Box, "/", newFolder, "/editedL0upload/deleteDuplicateInd_for2023release.txt", sep=''), 
            sep="\t", row.names = FALSE, col.names="uuid", na='')

#######

review <- read.csv('C:/Users/kjones/Desktop/forOffline_workingDir/pheOptimization/phePerInd_reviewTheseDups_07Sep2022.csv', stringsAsFactors = F)

write.csv(review, paste(l0Box, newFolder, 'originalL0download/phe_ind_dupesToDelete_review.csv', sep='/'), row.names = FALSE)

sort(names(review))

table(review$fate)

## DELETE
uuid_only <- select(review, uuid) #=uid)

write.table(uuid_only, paste(l0Box, "/", newFolder, "/editedL0upload/deleteDuplicateInd_2_for2023release.txt", sep=''), 
            sep="\t", row.names = FALSE, col.names="uuid", na='')

#### SEBA3

df_SEBA3 <- get.os.l0.by.query(stack='prod',
                             tab = 'DP0.10002.001:phe_perindividual_in',
                             fieldDataSearchStrings = "SEBA3",
                             fieldName="taxonID",
                             #tag="NEON.PLA.D14.JORN.06761",
                             #namedLocation = "BART",
                             # minEndDate = "2019-01-01",
                             # maxEndDate = "2020-01-01",
                             format_for_L0_editor=TRUE)

table(df_SEBA3$growthForm)

out <- filter(df_SEBA3, growthForm=='GRS')

write.csv(out, paste(l0Box, newFolder, 'originalL0download/seba3_gf_update.csv', sep='/'), row.names = FALSE)

out$growthForm <- "Forb"

write.table(out, paste(l0Box, "/", newFolder, "/editedL0upload/phe_seba_edited.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

#### ARFR4

df_ARFR4 <- get.os.l0.by.query(stack='prod',
                               tab = 'DP0.10002.001:phe_perindividual_in',
                               fieldDataSearchStrings = "ARFR4",
                               fieldName="taxonID",
                               format_for_L0_editor=TRUE)
#check phenophase names
si_ARFR4 <- get.os.l0.by.query(stack='prod',
                               tab = 'DP0.10002.001:phe_statusintensity_in',
                               fieldDataSearchStrings = "ARFR4",
                               fieldName="taxonID",
                               format_for_L0_editor=TRUE)

table(df_ARFR4$growthForm)
table(si_ARFR4$growthForm, si_ARFR4$phenophaseName)
table(si_ARFR4$individualID)

out <- filter(df_ARFR4, growthForm=='GRS')

write.csv(out, paste(l0Box, newFolder, 'originalL0download/ARFR4_gf_update.csv', sep='/'), row.names = FALSE)

out$growthForm <- "DDB"

write.table(out, paste(l0Box, "/", newFolder, "/editedL0upload/phe_ARFR4_edited.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

si_ARFR4$date[si_ARFR4$phenophaseName=="Initial growth"]
si_ARFR4$individualID[si_ARFR4$phenophaseName=="Initial growth"]

del_si <- si_ARFR4[si_ARFR4$individualID=="NEON.PLA.D10.CPER.06645" & si_ARFR4$date=="2021-06-17T06:00:00.000Z",]

write.csv(del_si, paste(l0Box, newFolder, 'originalL0download/ARFR4_si_badGF.csv', sep='/'), row.names = FALSE)

uuid_only <- select(del_si, uuid=uid)

write.table(uuid_only, paste(l0Box, "/", newFolder, "/editedL0upload/ARFR4_si_delBadGF.txt", sep=''), 
            sep="\t", row.names = FALSE, col.names="uuid", na='')

###
#### BOPE2

df_bope <- get.os.l0.by.query(stack='prod',
                          tab = 'DP0.10002.001:phe_perindividual_in',
                          fieldDataSearchStrings = "BOPE2",
                          fieldName="taxonID",
                          format_for_L0_editor=TRUE)
#check phenophase names
si_bope <- get.os.l0.by.query(stack='prod',
                               tab = 'DP0.10002.001:phe_statusintensity_in',
                               fieldDataSearchStrings = "BOPE2",
                               fieldName="taxonID",
                               format_for_L0_editor=TRUE)

table(df_bope$growthForm)
table(si_bope$growthForm, si_bope$phenophaseName)
table(si_bope$individualID)

out <- filter(df_bope, growthForm=='Forb')

write.csv(out, paste(l0Box, newFolder, 'originalL0download/bope_gf_update.csv', sep='/'), row.names = FALSE)

out$growthForm <- "GRS"

write.table(out, paste(l0Box, "/", newFolder, "/editedL0upload/phe_bope_edited.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

allEdits <- bind_rows(allEdits, out)

#

df_coun <- get.os.l0.by.query(stack='prod',
                          tab = 'DP0.10002.001:phe_perindividual_in',
                          fieldDataSearchStrings = "COUN",
                          fieldName="taxonID",
                          format_for_L0_editor=TRUE)
#check phenophase names
si_coun <- get.os.l0.by.query(stack='prod',
                          tab = 'DP0.10002.001:phe_statusintensity_in',
                          fieldDataSearchStrings = "COUN",
                          fieldName="taxonID",
                          format_for_L0_editor=TRUE)

table(df_coun$growthForm, df_coun$plotID)
table(si_coun$growthForm, si_coun$phenophaseName)
table(si_$individualID)

out <- filter(df_coun, growthForm=='DBL')

write.csv(out, paste(l0Box, newFolder, 'originalL0download/coun_gf_update.csv', sep='/'), row.names = FALSE)

si_out <- filter(si_coun, growthForm=='DBL')

write.csv(si_out, paste(l0Box, newFolder, 'originalL0download/coun_statint_update.csv', sep='/'), row.names = FALSE)

out$growthForm <- "Forb"

write.table(out, paste(l0Box, "/", newFolder, "/editedL0upload/phe_coun_edited.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

out <- rename(out, Data.Product='Data Product')

allEdits <- rbind(allEdits, out)


del_si <- si_out[si_out$phenophaseName%in%c('Colored leaves', 'Falling leaves', 'Increasing leaf size'),]

del_si <- si_[si_$individualID=="" & si_$date=="",]

write.csv(del_si, paste(l0Box, newFolder, 'originalL0download/coun_si_deletebadGF.csv', sep='/'), row.names = FALSE)

si_out <- si_out[si_out$phenophaseName%in%c('Breaking leaf buds', 'Leaves', 'Open flowers'),]

write.csv(del_si, paste(l0Box, newFolder, 'originalL0download/coun_si_editbadGF.csv', sep='/'), row.names = FALSE)


uuid_only <- select(del_si, uuid=uid)

write.table(uuid_only, paste(l0Box, "/", newFolder, "/editedL0upload/coun_si_delBadGF.txt", sep=''), 
            sep="\t", row.names = FALSE, col.names="uuid", na='')


si_out$growthForm <- 'Forb'
si_out$phenophaseName[si_out$phenophaseName=="Breaking leaf buds"] <- 'Initial growth'
si_out$phenophaseIntensityDefinition[si_out$phenophaseName=='Initial growth'] <- NA

write.table(si_out, paste(l0Box, "/", newFolder, "/editedL0upload/phe_si_coun_edited.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')
#

#### ERUM

df_erum <- get.os.l0.by.query(stack='prod',
                          tab = 'DP0.10002.001:phe_perindividual_in',
                          fieldDataSearchStrings = "ERUM",
                          fieldName="taxonID",
                          format_for_L0_editor=TRUE)
#check phenophase names
si_erum <- get.os.l0.by.query(stack='prod',
                          tab = 'DP0.10002.001:phe_statusintensity_in',
                          fieldDataSearchStrings = "ERUM",
                          fieldName="taxonID",
                          format_for_L0_editor=TRUE)

table(df_erum$plotID, df_erum$growthForm)
table(si_erum$growthForm, si_erum$phenophaseName)
table(si_erum$individualID)

out <- filter(df_erum, growthForm=='Forb')

write.csv(out, paste(l0Box, newFolder, 'originalL0download/erum_gf_update.csv', sep='/'), row.names = FALSE)

out$growthForm <- "DDB"

write.table(out, paste(l0Box, "/", newFolder, "/editedL0upload/phe_erum_edited.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

out <- rename(out, Data.Product='Data Product')

allEdits <- rbind(allEdits, out)

si_out <- si_erum[si_erum$growthForm=="Forb",]

si_out$growthForm <- "DDB"

si_out$phenophaseName[si_out$phenophaseName=="Initial growth"] <- "Young leaves"

write.table(si_out, paste(l0Box, "/", newFolder, "/editedL0upload/phe_si_erum_edited.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

#### MIRE - NOT DONE!!!

df_mire <- get.os.l0.by.query(stack='prod',
                          tab = 'DP0.10002.001:phe_perindividual_in',
                          fieldDataSearchStrings = "MIRE",
                          fieldName="taxonID",
                          format_for_L0_editor=TRUE)
#check phenophase names
si_mire <- get.os.l0.by.query(stack='prod',
                          tab = 'DP0.10002.001:phe_statusintensity_in',
                          fieldDataSearchStrings = "MIRE",
                          fieldName="taxonID",
                          format_for_L0_editor=TRUE)

table(df_mire$growthForm)
table(si_mire$growthForm, si_mire$phenophaseName)
table(si_$individualID)

out <- filter(df_, growthForm=='')

write.csv(out, paste(l0Box, newFolder, 'originalL0download/_gf_update.csv', sep='/'), row.names = FALSE)

out$growthForm <- ""

write.table(out, paste(l0Box, "/", newFolder, "/editedL0upload/phe__edited.txt", sep=''), 
            sep="\t", row.names = FALSE, na='')

out <- rename(out, Data.Product='Data Product')

allEdits <- rbind(allEdits, out)

si_erum <- si_erum[si_erum$growthForm=="Forb",]

si_$individualID[si_$phenophaseName=="Initial growth"]

del_si <- si_[si_$individualID=="" & si_$date=="",]

write.csv(del_si, paste(l0Box, newFolder, 'originalL0download/_si_badGF.csv', sep='/'), row.names = FALSE)

uuid_only <- select(del_si, uuid=uid)

write.table(uuid_only, paste(l0Box, "/", newFolder, "/editedL0upload/_si_delBadGF.txt", sep=''), 
            sep="\t", row.names = FALSE, col.names="uuid", na='')

#

## start again with PIGR4







write.csv(allEdits, 'C:/Users/kjones/Desktop/temp/phe_perind_updates.csv', row.names=F)

fidsToDelete <- c(fidsToDelete, del)
length(unique(fidsToDelete))
fidsToDelete <- unique(fidsToDelete)

saveRDS(fidsToDelete, 'C:/Users/kjones/Documents/GitHub/NEON-OS-optimization/phenology/dataCleaning/for2022release/fidsToDelete.rds')

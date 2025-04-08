library(devtools)
#devtools::install_local('C:/Users/kjones/Documents/GitHub/how-to-make-a-data-product/REST_R/restR',
#                        force = TRUE)

#devtools::install_github('NEONScience/restR2', force = T)

library(restR)
library(restR2)

api_token <- Sys.getenv("FULCRUM_KEY")

#ltr = dpID="DP0.10033.001"
#phe = dpID= "DP0.10002.001"
#vst - dpID= "DP0.10098.001"
#scs - "DP0.10000.001"
#fsp = DP0.30012.001


##### get L0 record
#parallel for large pulls
df_phe <- par.get.os.l0.data(stack='prod',
                         dpID = 'DP0.10002.001',
                         startDate = '2023-01-01',
                         endDate = '2025-03-01',
                         ingestTable='phe_perindividualperyear_in',
                         format_for_L0_editor=TRUE)


df_vst <- par.get.os.l0.data(stack='prod',
                             dpID = 'DP0.10098.001',
                             startDate = '2014-01-01',
                             endDate = '2024-01-01',
                             ingestTable='vst_mappingandtagging_in',
                             format_for_L0_editor=TRUE)

# for regular pulls
df <- get.os.l0.data(stack='prod',
                     dpID = 'DP0.10002.001', 
                     ingestTable='phe_perindividualperyear_in',
                     #activityUUID = 'a802895a-a97f-4ad6-940b-8b01e2d148d0',
                     namedLocationName="BONA",
                     inclDescendants = "true",
                     #stringSearch= "taxonID:CADE27",
                     #inclDescendants = TRUE,
                     #sampleTag="NEON.PLA.D19.HEAL.06193",
                     format_for_L0_editor=TRUE)

#for list of fids
fids=c("96bfc0e9-e980-48e8-b163-89e76c2ce30e",
       "b1686776-7531-4ceb-9efc-c7d13b5b55fc",
       "a5c00ff6-fe85-4b4c-88a1-04290c357f80")

df_si <- data.frame()

for (i in tags){
  temp <- get.os.l0.data(stack='prod',
                     dpID = 'DP0.10002.001', 
                     ingestTable='phe_statusintensity_in',
                     #stringSearch=paste0('fulcrumID:', i),
                     #activityUUID = 'a802895a-a97f-4ad6-940b-8b01e2d148d0',
                     #namedLocationName="YELL",
                     #inclDescendants = "true"
                     #inclDescendants = TRUE,
                     sampleTag= i,
                     #minStartDate = "2023-01-01",
                     #maxStartDate = "2022-12-01",
                     #minTransactionDate="2020-10-01",
                     #maxTransactionDate="2021-06-01",
                     format_for_L0_editor=TRUE)
  df_si <- bind_rows(df_si, temp)
}



table(df_look$growthForm, useNA="ifany")
df_look$addDate

write.csv(df,  "C:/Users/kjones/OneDrive - National Ecological Observatory Network/Desktop/temp/D03_annual.csv")



### in a for loop ###
missingTags <- c("NEON.PLA.D05.TREE.00427", "NEON.PLA.D16.WREF.04934")
df_mt <- data.frame()

for (i in missingTags){
  print(i)
  temp <- get.os.l0.by.query(stack='prod',
                         tab = 'DP0.10098.001:vst_mappingandtagging_in',
                         #fieldDataSearchStrings = missingTags,
                         #fieldName="individualID",
                         tag=i,
                        #namedLocation = "BART",
                         #minEndDate = "2022-01-01",
                         #maxEndDate = "2022-10-01",
                         format_for_L0_editor=TRUE)
    df_mt <- bind_rows(df_mt, temp)
    rm(temp)
}

unique(df$eventID)


write.csv(df, 'C:/Users/kjones/Desktop/temp/phe_ind_badGF.csv', row.names=FALSE)
###

df_loc <- get.os.l0.by.namedLocation(stack='prod',
                                     tab="DP0.10002.001:phe_statusintensity_in",
                                     pullType = 'startDate', 
                                     minDate = '2019-06-01',
                                     maxDate = '2020-01-01',
                                     namedLocationName = "JERC")

df_si <- data.frame()

##### get L0 records by tagID, loop
ids <- unique(df$individualID)

for(i in ids){
  print(i)
  temp <- get.os.l0.all.opts(stack='prod', 
                             tab="DP0.10002.001:phe_statusintensity_in",
                             tag=i,
                             format_for_L0_editor=TRUE)
  df_si <- rbind(df_si, temp)
}

tags <- ids[!is.na(ids)]


###### get message admin messages

me <- get.os.messages(stack = "prod",
                       status = "FAILED_VALIDATION",
                       fileURI = "ccd89dbc-26cf-4d7c-8581-75370a2c4598/1ca73aba-2551-433c-9847-7aad76c2d2fd",
                       messageUUID = message_manual)

mess <- get.os.messages(
  stack = "prod",
  messageUUID = "9a7b31e7-5df5-4087-88a0-be21beb887d7",
  #ingestTableKey = NA,
  #locationName = NA,
  out = "dataframe"
)

#### get taxon info

tax <- find.taxon.info(type="PLANT", stack='prod', taxonID="ACNE4") #SEGR4
find.scientific.name(type="PLANT", stack='prod', taxonID='ERSI') #SEGR4
find.accepted.taxon.id(type="PLANT", stack='prod', scientificName = 'Senegalia greggii (A. Gray) Britton & Rose')

#taxon ID list
tax <- get.taxon.table(
  type = 'PLANT',
  taxonID_list = c('ACCOC', 'ACNE4'))

names(tax)

tax$taxonID[1] <- "VACO9"
tax$acceptedTaxonID[1] <- "VACO9"
tax$scientificName[1] <- "Vachellia constricta (Benth.) Seigler & Ebinger"
tax$scientificNameAuthorship[1] #no need to change, already Benth.
tax$taxonRank[1] <- 'species'
tax$vernacularName[1] #same
tax$family[1] #same
tax$genus[1] <- 'Vachellia'
tax$specificEpithet[1] #same
tax$infraspecificEpithet[1] <- NA

tax$taxonID[2] <- "VAVE"
tax$acceptedTaxonID[2] <- "VAVE"
tax$scientificName[2] <- "Vachellia vernicosa (Britton & Rose) Seigler & Ebinger"
tax$scientificNameAuthorship[2] <- "Britton & Rose"
tax$taxonRank[2] #same
tax$vernacularName[2] #same
tax$family[2] #same
tax$genus[2] <- "Vachellia"
tax$specificEpithet[2] <- 'vernicosa'
tax$infraspecificEpithet[2] #same

write.csv(tax, 'C:/Users/kjones/Documents/GitHub/NEON-OS-taxonomy/Updates/PLANT/20220510 -- RITM0030796.csv', row.names = F)


### get L1 data ###
df_l1 <- get.os.l1(stack='prod',
                tab='DP1.10055.001:phe_perindividual_pub',
                startDate = 2013-01-01,
                endDate = 2021-04-05
                )

df_l1 <- get.os.l1.by.tab.all.opts(stack="prod", 
                                   tab="DP1.10055.001:phe_perindividual_pub",
                                   tag="NEON.PLA.D03.JERC.06792")

df_byMes <- get.os.l0(pullType = "messageUuid",
                stack = 'prod',
                tab = 'DP0.10098.001:vst_apparentindividual_in',
                msgUuid= "4fc52126-97b7-4680-ba59-a214c7c3e179")

ful_df <- get.fulcrum.data(api_token = Sys.getenv('FULCRUM_KEY'),
                           appName="PHE: Per Individual INGEST[PROD]", 
                           #repeatable='phe_pertag',
                           fulcFields_oi = c('domainid', 'siteid', 'taxonid', 'growthform', 'load_status'),
                           queryField = 'taxonid',
                           queryValues = 'COCA13') 
                           #domains = c('D03'))
                           #createdDateStart = '2022-01-01')
                           #createdDateEnd ='2021-12-01')

ful_df <- filter(ful_df, plottype=='tower')


for (i in 1:nrow(ful_df)){
  ful_df$scientificName[i] <- find.scientific.name(taxonID=ful_df$taxonid_pull[i],
                                              type = "PLANT")
}

df_3 <- filter(df, substr(plotID, 1, 8)%in%ful_df$plotid & yearBoutBegan>2018)
table(df_3$plotID, df_3$yearBoutBegan)
write.csv(df_3, 'C:/Users/kjones/Desktop/temp/d03_pmd_tower_2019to2021.csv', row.names=F)


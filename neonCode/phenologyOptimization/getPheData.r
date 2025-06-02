# Author: Natalie Robinson (nrobinson@battelleecology)
#Consultants - Katie Jones, Margaret Kosmola, Sarah Elmendorf, Andrew Finley, Claire Lunch
#During data subset, pay attention to 'missing' and making sure it passes in a way that represents how data would be collected under
#  reduced sampling

#set paths, source scripts, load libraries ----------------------------------------------------------
if (file.exists(
  'C:/Users/nrobinson')){
  wdir<-'C:/Users/nrobinson/Desktop/MyDocuments/NEON_Git'
  pathToOpt <-'C:/Users/nrobinson/Desktop/MyDocuments/NEON_Git/NEON-OS-optimization/phenology/phe_optimizationReport'
}

library(httr) # talk to the api
library(jsonlite)
library(XML)
library(plyr)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(grid)
library(lubridate)
library(neonUtilities)
options(warn=1)

detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

#For getting L0 data
get_pdrByBatch <- function (inDF,outDF){
  batchUrl <- paste0(env,'activities?include-field-data=true&include-samples=true&uuid=',
                     paste0(inDF$uuid,collapse = '&uuid='))
  batchReq = GET(batchUrl, httr::add_headers(Accept = "application/vnd.neoninc.os.activity-list-v1.0+xml"))
  batchCont = content(batchReq, as = "text", encoding = "UTF-8")
  batchDoc = xmlParse(batchCont)
  #Add to dataframe
  for (id in inDF$uuid){
    header <- xpathSApply(batchDoc, paste0("//activity[@uuid='",id,"']/fieldDatum/fieldName"), xmlValue)
    #Get fields at child level in json
    recDat <- data.frame(t(xpathSApply(batchDoc, paste0("//activity[@uuid='",id,"']/fieldDatum"), function(x) {
      if (xpathSApply(x, "boolean(./stringValue)")) {
        xpathSApply(x, "./stringValue", xmlValue)
      } else if (xpathSApply(x, "boolean(./dateValue)")) {
        xpathSApply(x, "./dateValue", xmlValue)
      }else if (xpathSApply(x, "boolean(./numberValue)")) {
        xpathSApply(x, "./numberValue", xmlValue)
      }else{
        NA
      }
    })),stringsAsFactors = F)
    colnames(recDat) <- header
    # Add fields from header and sample info
    recDat$uuid <- id
    recDat$plotID <- xpathSApply(batchDoc, paste0("//activity[@uuid='",id,"']/namedLocationName"), xmlValue)
    recDat$date <- xpathSApply(batchDoc, paste0("//activity[@uuid='",id,"']/startDate"), xmlValue)
    recDat$endDate <- recDat$date
    recDat$Data.Product <- xpathSApply(batchDoc, paste0("//activity[@uuid='",id,"']/dataProductId"), xmlValue)
    recDat$Table <- xpathSApply(batchDoc, paste0("//activity[@uuid='",id,"']/ingestTableName"), xmlValue)
    recDat$Primary.Sample.Class <- xpathSApply(batchDoc,paste0("//activity[@uuid='",id,"']/primarySample/sampleInfo/sampleClassCode"),xmlValue)
    indID <- xpathSApply(batchDoc,paste0("//activity[@uuid='",id,"']/primarySample/sampleInfo/tag"),xmlValue)
    recDat$individualID <- ifelse(!is.null(indID) & !is.list(indID),indID,NA)
    #Bind to whole dataset
    outDF <- bind_rows(outDF,recDat)
  }
  return(outDF)
}

#detachAllPackages()

#Get status intensity data ----------------------------------------------------------
##################### From L0 - SLOW!! ##################################
#env = "http://prod-os-ds-1.ci.neoninternal.org:8080/osDataService/"
# sites = c('BART','HARV','BLAN','SERC','SCBI','DSNY','JERC','OSBS','GUAN','LAJA','STEI','TREE','UNDE',
#           'KONA','UKFS','KONZ','GRSM','MLBS','ORNL','DELA','LENO','TALL','DCFS','NOGP','WOOD','CPER',
#           'RMNP','STER','CLBJ','OAES','YELL','MOAB','NIWO','JORN','SRER','ONAQ','ABBY','WREF','SJER',
#           'SOAP','TEAK','BARR','TOOL','BONA','DEJU','HEAL','PUUM')
# 
# # Make this a function so it's performed in a loop over domains - NEEDS TO GET L0
# dtStrings <- c('&start-date-begin=2013-01-01T00:00:00.000Z&start-date-cutoff=2013-12-31T00:00:00.000Z',
#                '&start-date-begin=2014-01-01T00:00:00.000Z&start-date-cutoff=2014-12-31T00:00:00.000Z',
#                '&start-date-begin=2015-01-01T00:00:00.000Z&start-date-cutoff=2015-12-31T00:00:00.000Z',
#                '&start-date-begin=2016-01-01T00:00:00.000Z&start-date-cutoff=2016-12-31T00:00:00.000Z',
#                '&start-date-begin=2017-01-01T00:00:00.000Z&start-date-cutoff=2017-12-31T00:00:00.000Z',
#                '&start-date-begin=2018-01-01T00:00:00.000Z&start-date-cutoff=2018-12-31T00:00:00.000Z',
#                '&start-date-begin=2019-01-01T00:00:00.000Z&start-date-cutoff=2019-12-31T00:00:00.000Z')
# allCombos <- apply(expand.grid(sites, dtStrings), 1, paste, collapse="_")
# 
# #2013 = 1:47;2014= 48:94;2015=95:141;2016=142:188,2017=189:235;2018=236:len(allCombos)
# 
# t1=proc.time()
# allDat <- data.frame(); dropDat <- data.frame(); tooMany <- vector(); noRecs <- vector()
# for (ac in allCombos){  #41 mins for ORNL 2015
#   print (ac)
#   s <- unlist(strsplit(ac,'_'))[1]
#   ds <- unlist(strsplit(ac,'_'))[2]
#   datUrl <-  paste0(env,'activities?ingest-table-key=NEON.DOM.SITE.DP0.10002.001:phe_statusintensity_in&parent-named-location-name=',
#                     s,ds,'&include-field-data=false&include-samples=false')
#   datReq = GET(datUrl, httr::add_headers(Accept = "application/vnd.neoninc.os.activity-list-v1.0+xml"))
#   datCont = content(datReq, as = "text", encoding = "UTF-8")
#   if (grepl('too many',datCont)){
#     tooMany <- c(tooMany,ac)
#   }else if (grepl('count="0"',datCont)){
#     noRecs <- c(noRecs,ac)
#   }else{
#     #Get uuids with startDate in this site/time period
#     datDoc = xmlParse(datCont)
#     theIDs=cbind(
#       data.frame(uuid=xpathSApply(datDoc, "//activity", xmlGetAttr, 'uuid')),
#       xmlToDataFrame(nodes = getNodeSet(datDoc, "//activity"),stringsAsFactors =F)
#     ); theIDs$uuid <- as.character(theIDs$uuid)
#     #Pull as batch and add to dataframe (pull as batch and then parse is ~ 66% faster than pull one-by-one)
#     recs <- data.frame()
#     if (nrow(theIDs) < 1000){
#       recs <- get_pdrByBatch(theIDs,recs)
#     }else{
#       brks <- seq(1, nrow(theIDs), 999)
#       #Chunk the data and pull/add to dataframe
#       for (chunk in 1:length(brks)){
#         if (chunk == 1){
#           datChunk <-theIDs[1:999,]
#         }else if (chunk==length(brks)){
#           datChunk <-theIDs[(brks[chunk]+1):nrow(theIDs),]
#         }else{
#           datChunk <-theIDs[brks[chunk]:brks[chunk+1],]
#         }
#         recs <- get_pdrByBatch(datChunk,recs)
#       }
#     }
#     #de-dup
#     if (! 'individualID' %in% names(recs)){
#       print(head(recs))
#     }
#     recs <- mutate(recs,'temp'=paste(individualID,substring(date,1,10),phenophaseName,measuredBy,sep='.')) 
#     keepDat <- recs[!duplicated(recs[c("temp")]) & !duplicated(recs[c("temp")],fromLast = T), ]
#     drp <- data.frame()
#     dups <- recs[duplicated(recs[c("temp")]) | duplicated(recs[c("temp")],fromLast = T), ]
#     for (tmp in unique(dups$temp)){
#      sub <- filter(recs,temp==tmp) %>% arrange(editedDate)
#      keepDat <- bind_rows(keepDat, sub[nrow(sub),])
#      drp <-  bind_rows(drp,sub[1:(nrow(sub)-1),]) #
#     }
#     #Add to allDat and dropDat
#     allDat <- bind_rows(allDat,keepDat)
#     dropDat <- bind_rows(dropDat,drp)
#   }
# }
# 
# print(proc.time()-t1)  #Takes about 124 hours to get all data
# 
# #nothing in tooMany 
# 
# allDat <- distinct(allDat) #2261556 to 2261556. CI reports 2285851

##################### From L1 ##################################
# Get all data with neonUtilities
allDat <- loadByProduct(dpID='DP1.10055.001')
statInt <- allDat$phe_statusintensity
perInd <- allDat$phe_perindividual
perIndPerYr <- allDat$phe_perindividualperyear
rm(allDat)

# Find dups by primary key
nrow(filter(statInt,is.na(individualID)))  #No rows with missing individualID
statInt <- mutate(statInt,'temp'=paste(individualID,date,phenophaseName,measuredBy,sep='.'))

length(which(!duplicated(statInt$temp) & !duplicated(statInt$temp,fromLast = T)))
statInt_dup <- filter(statInt,duplicated(temp) | duplicated(temp,fromLast = T))  #Get dups only
statInt_noDup <- filter(statInt,!duplicated(temp) & !duplicated(temp,fromLast = T)) #Get non-dups only

#Add most recently edited of each duplicated record back to main dataframe -takes forever
drp <- data.frame()
for (tmp in unique(statInt_dup$temp)){
  sub <- filter(statInt_dup,temp==tmp) %>% arrange(editedDate)
  statInt_noDup <- bind_rows(statInt_noDup, sub[nrow(sub),])
  drp <-  bind_rows(drp,sub[1:(nrow(sub)-1),]) #
}

#save drp
write.csv(drp,'C:/Users/nrobinson/Desktop/drpDataframe.csv',row.names = F)

#Make sure everything is in drp and then delete statInt_dup - LEFT OFF HERE
nrow(drp)
which(!statInt_dup$temp %in% drp$temp)


#Add fields not grabbed but needed for upload to pdr
allDat <- allDat[,-which(names(allDat)=='temp')];dropDat <- dropDat[,-which(names(dropDat)=='temp')]
allDat$individualBarcode <- NA; dropDat$individualBarcode <- NA
allDat$dataQF <- 'legacyData'; dropDat$dataQF <- 'legacyData'
allDat$assignedTo <- NA; dropDat$assignedTo <- NA

#save.image(paste0(path???ToOpt,'/pheAnalysis_allDat.RData'))  
nrow(dropDat[is.na(dropDat$editedDate),])   #2

#Data for Katie J --------------------------------------------------------------
#If 'commissioning' is in remarks, delete from dropDat (look for misspellings)
#write.table(filter(dropDat,!grepl('commis',tolower(remarks))),'C:/Users/nrobinson/Desktop/pheStatIntens_delFromPDR.txt',sep = ',', col.names = TRUE, row.names = F, quote = T, na = '')

#Records with no day of year - to be updated in PDR
#write.table(allDat[is.na(allDat$dayOfYear),],'C:/Users/nrobinson/Desktop/pheStatIntens_missingDOY.txt',sep = ',', col.names = TRUE, row.names = F, quote = T, na = '')

#Clean data -----------------------------------------------------------
#Make sure records without editedDate are handled properly
#Go through these with KJ. There are no records without editedDate in dropDat (yay!). 
#For each individualID/date combo here, decide what to do with records in allDat (looks like some are fulcrum and others spreadsheet upload, so prob dupes)
noEditDate <- allDat[is.na(allDat$editedDate),]   

#Drop uncertain phenophaseStatus observations, select out uid, measuredBy, recordedBy, dataQF
allDat <- select(allDat,names(allDat)[!names(allDat) %in% c("clientCreatedDate","createdBy","createdDate","dataEntryAltitude",
                                                            "dataEntryLatitude","dataEntryLongitude","measuredBy","platformInfo",
                                                            "recordedBy","recordVersion","status","temp","updatedBy","updatedDate",
                                                            "samplingProtocolVersion","project","fulcrumVersion","uuid",
                                                            "uid","horizontalAccuracy","dataQF","assignedTo","Primary.Sample.Class",
                                                            "Data.Product","Table")]) %>%
  filter(phenophaseStatus != 'uncertain')  #2229975 records

#Check for records with missing: growthForm, date, phenophaseName, phenophaseStatus, individualID, plotID
nrow(filter(allDat,is.na(phenophaseIntensity))) #798 records with no growthForm; all others are fine. 

#Remove descoped sites and records with no growthForm
allDat <- filter(allDat,!substring(plotID,1,4) %in% c('STEI','KONA','DCFS','STER'))  #1904749 records left
allDat <- filter(allDat,!is.na(growthForm))  #2039294 records left

#Align data
allDat$growthForm[allDat$growthForm=='Deciduous broadleaf'] <- 'DBL'
allDat$growthForm[allDat$growthForm=='Deciduous conifer'] <- 'DC'
allDat$growthForm[allDat$growthForm=='Drought deciduous broadleaf'] <- 'DDB'
allDat$growthForm[allDat$growthForm=='Evergreen broadleaf'] <- 'EB'
allDat$growthForm[allDat$growthForm=='Evergreen conifer'] <- 'EC'
allDat$growthForm[allDat$growthForm=='Semi-evergreen broadleaf'] <- 'SEBR'
allDat$growthForm[allDat$growthForm=='Graminoid'] <- 'GRS'

#Add seasonality column ('y' == seasonal, 'n' == yr round OR hybrid (ONAQ,NIWO,MOAB))
allDat$seasonal <- ifelse(substring(allDat$plotID,1,4) %in% c('BART','HARV','BLAN','SCBI','SERC','TREE','UNDE','KONZ','GRSM','MLBS',
                                                              'UKFS','ORNL','NOGP','WOOD','RMNP','YELL','ABBY','WREF','TEAK','BARO',
                                                              'TOOL','BONA','DEJU','HEAL'),'y','n')

#add phaseNum column so models can be run by reasonable transitions
allDat$phaseNum <- ifelse(allDat$phenophaseName%in% c('Breaking leaf buds','Breaking needle buds','Initial growth','Emerging needles'),'p1',
                          ifelse(grepl('Young',allDat$phenophaseName),'p2',
                                 ifelse(allDat$phenophaseName =='Increasing leaf size','p2b',
                                        ifelse(grepl('Open',allDat$phenophaseName),'p4',
                                               ifelse(grepl('Colored',allDat$phenophaseName),'p5',
                                                      ifelse(grepl('Falling',allDat$phenophaseName),'p6','p3'))))))

#Update DOY - some are missing, others are incorrect
allDat$ind <- row.names(allDat)  #Index for complex data filtering/subsetting later
allDat$year <- substring(allDat$date,1,4)
allDat$dayOfYear <- as.character(yday(as.Date(substring(allDat$date,1,10),format='%Y-%m-%d')))

save.image(paste0(pathToOpt,'/pheAnalysis_processedDat.RData'))  #Last save = allDat includes all processing until here
#load(paste0(pathToOpt,'/pheAnalysis_processedDat.RData'))

#Get most recent taxonID for individual from L1  -----------------------------------------------------------
#env='http://prod-os-ds-1.ci.neoninternal.org:8080/osDataService/'

for (id in unique(allDat$individualID)){
  #Get data from L1
  indUrl <- paste0(env,'results?sample-tag=',id,'&include-result-data=true&include-samples=true')
  indReq <-  httr::GET(indUrl, httr::add_headers(Accept = "application/vnd.neoninc.os.result-list-v1.0+xml"))
  indCont = httr::content(indReq, as = "text", encoding = "UTF-8")
  indDoc <- XML::xmlParse(indCont)
  #Get editedDates and taxonIDs of per_individual records, keep latest entry. Don't consider individuals with no perIndividual data
  if(is.null(XML::getNodeSet(indDoc, "//result[pubTableName='phe_perindividual_pub']/resultDatum"))){
    allDat <- filter(allDat,!allDat$individualID==id)
  }else{
    perIndDat <- XML::xmlToDataFrame(indDoc, nodes=XML::getNodeSet(indDoc, "//result[pubTableName='phe_perindividual_pub']/resultDatum"))
    perIndDat[is.na(perIndDat)] <- ""
    perIndDat$value <- do.call(paste, c(perIndDat[2:4], sep="")); perIndDat <- perIndDat[,c(1,5)]
    theDat <-  group_by(perIndDat,fieldName) %>% mutate(ind = row_number()) %>% spread(fieldName, value) %>% arrange(editedDate)
    allDat$taxonUpdate[allDat$individualID==id] <- theDat$taxonID[nrow(theDat)]
  }
}

save.image(paste0(pathToOpt,'/pheAnalysis_processedDat.RData'))  #Last save = allDat includes all processing until here
write.csv(allDat,'C:/Users/nrobinson/Desktop/pheStatIntens_allDatWithTaxa.csv', row.names = F, na = '')

#Make sure taxonID is consistent for each individualID
# for (i in unique(allDat$individualID)){
#  if (length(unique(allDat$taxonUpdate[allDat$individualID==i])) > 1){
#    print (i)
#  }
# }

#if (!require("neonUtilities")) install.packages("neonUtilites")
library(neonUtilities)
library(neonOS)

#library(usethis)
# to edit environmental variables
#usethis::edit_r_environ()


# library(devtools)
# install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
# library(geoNEON)


#ltr = dpID="DP1.10033.001"
#phe = dpID= "DP1.10055.001"
#vst = dpID="DP1.10098.001"
#cdw = dpID="DP1.10010.001
#nst = dpID="DP1.10045.001"
#hbp = dpID= "DP1.10023.001"
#event = "DP1.10111.001"
#div = dpID="DP1.10058.001"

##
dat <- loadByProduct(dpID="DP1.10055.001",
                    #tabl="phe_perindividualperyear",
                    #site = c("BONA", "HEAL"),
                    startdate = "2024-01",
                    enddate = "2024-12",
                    #package = "basic",
                    include.provisional = TRUE,
                    release = "LATEST",
                    #release = "current",
                    check.size = FALSE, 
                    #token = Sys.getenv('latestTok'))
                    token = Sys.getenv('NEON_PAT'))

# unlist all data frames
list2env(dat ,.GlobalEnv)


dec <- ltr_fielddata%>%
  filter(substr(collectDate, 6, 7)=="12")




phe_perindividual$growthForm[phe_perindividual$taxonID=="VEOF2"]

#*COCA13   *COUN   FRVI   GETR   *LEVU   *MYMU  *PIGR4  POIN5  *VEOF2
#write.csv(phe_perindividual, "phe_perindidividual.csv", row.names=F)

# taxTable <- getTaxonTable(taxonType = "PLANT", token=Sys.getenv('NEON_KEY'))
# 
# 

plotBySite <- ltr_pertrap %>%
  group_by(siteID) %>%
  summarise(plotCount = n_distinct(plotID), 
            eCount=n_distinct(trapID[trapType=="Elevated"]),
            gCount=n_distinct(trapID[trapType=="Ground"]),
            yearCount=n_distinct(year))


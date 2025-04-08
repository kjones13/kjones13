library(dplyr)

trees <-  ### your filtered and joined dataset of tree measurements
plots <- read.csv("filesToStack10098/stackedFiles/vst_perplotperyear.csv", 
                  stringsAsFactors = F) ## replace with your filepath

subset_plot <- plots%>%
  filter(plotID%in%trees$plotID & eventID%in%trees$eventID)%>%  #only keep plot records that match plotID+eventID combo in trees df
  distinct(siteID, plotID, plotType, totalSampledAreaTrees) # remove duplicates and select fields to include

## These numbers should match
length(unique(subset_plot$plotID))
length(unique(trees$plotID))

## If the numbers are different 
missingPlots <- setdiff(trees$plotID, subset_plot$plotID) ##submit feedback on NEON data portal that records missing from vst_perplotperyear.
# then remove plots that you do not know the sampling area from trees dataset
trees <- filter(trees, !plotID%in%missingPlots)


#summarize sampling area by site and plotType
sampleAreaSummary <- subset_plot%>%
  group_by(siteID, plotType)%>%
  summarise(totalSampledPlotArea=sum(totalSampledAreaTrees))


# find Dec eventID errors
# Clean up workspace
rm(list = ls())

#load neonArrow
#devtools::install_github("NEONScience/NEON-OS-data-products-team/dev_cloud_workflows/neonArrow")

# load packages
library(neonArrow)
library(gargle)
library(dplyr)
library(neonUtilities)

# get GCS token
my_gcs_token = gargle::token_fetch(
  scope='https://www.googleapis.com/auth/cloud-platform')

2

# NEON dpID for litter
my_dpid <- "DP1.10055.001"


# data product table
my_tabl <- "phe_perindividualperyear"

# sites to try
# my_site_list <- "MLBS"
# other options to try:
# my_site_list <- c("SRER","GUAN","MLBS","JERC")
 my_site_list <- "all"

# create active binding for dataset
ds <- open_neon_dataset(
  dpID = my_dpid,
  site = my_site_list,
  release = "LATEST",
  package = "basic",
  tabl = my_tabl,
  partitioning_fields = c("siteID","collectDate"),
  token = Sys.getenv("NEON_PAT"),
  gcs_token = my_gcs_token)

# view names
ds %>% names()

# collect the full dataset to compare against neonUtilities download
dat_arrow <- ds |> collect()

# # get neonUtilities LATEST data
# neon_utils_data_return <- loadByProduct(
#   dpID = my_dpid,
#   site = my_site_list,
#   release = "LATEST",
#   package = "basic",
#   tabl = my_tabl,
#   token = Sys.getenv("NEON_PAT"),
#   check.size = FALSE)
#
# # extract the table
# dat_neon_utils <- neon_utils_data_return[[my_tabl]]
#
# # compare no. recs (they should be the same)
# dat_arrow |> nrow()
# dat_neon_utils |> nrow()

# check for diff uids, should return character(0)
#symdiff(dat_arrow$uid, dat_neon_utils$uid)

fix <- dat_arrow[grepl(pattern = ")", x = dat_arrow$eventID)==TRUE,]

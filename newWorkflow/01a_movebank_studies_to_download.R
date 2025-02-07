# ---
# title: "MoveTraits Database"
# author: "Anne K. Scharf"
# date: "February 2025"
# ---

## this script:
### gatherers all studies to download, 
### those shared with the specific movebank user 
### and those that are publicly available
### metadata table is created and saved including study name, owner, license terms, download date, etc

#######################################
##### ToDo: 
## merge limesurvey table with "all_shared" table to assign optout/optin of resolution to publish
## add row ID to metadata table, this rowID should than be displayed in the final table, to know from which studies the traits were calculated
#######################################

library(move2)
library(units)
library(dplyr)

# keyring::key_list()
options("move2_movebank_key_name" = "MoveTraits")

dir.create("MoveTraitsData")
pathTOfolder <- "./MoveTraitsData/"

#### downloading studies to which the user "MoveTraits" has been added as collaborator or manager
# download list of studies available through this account
all_shared <- movebank_download_study_info(study_permission=c("data_manager","collaborator"))
# all_shared$xxx <- xxx ## add variables gained from the lime survey, e.g. shared with Movetraits user, which resolution choice, etc
all_shared$access <- "MoveTraits"

### searching for public studies
all <- movebank_download_study_info() # some studies have years in weird formats, just ignore this warning message
all <- all[grep("GPS", all$sensor_type_ids),] # studies can have multiple sensors, making sure gps is included
all <- all[all$number_of_deployed_locations > units::set_units(0,"count"),] # removing those with 0 locations
all <- all[!is.na(all$number_of_deployed_locations),] # removing those with no deployed locations
all <- all[!is.na(all$taxon_ids),] ## removing those with NO taxon
all <- all[!all$is_test==T,] ## removing studies marked as tests
all_open <- all[which(all$license_type %in% c("CC_0","CC_BY","CC_BY_NC")),] 
## - CC_O: can use the data, do not need to mention names
## - CC_BY: can use the data, but names of owners should appear somewhere
## - CC_BY_NC: can use the data, but names of owners should appear somewhere
all_open$access <- "open"

### making one large table and removing duplicated studies
allstudies <- rbind(all_shared,all_open)
allstudies <- allstudies[!duplicated(allstudies$id),] ## when duplicated, entry from all shared will be kept
allstudies$download_date <- Sys.Date()
## for testing
# textstd <- c(404939825,183770262,1764627) # 2 studies of eidolon sharing same indiv, 1 study of buffalo w/ multiple deployments
# allstudies <- allstudies[allstudies$id%in%textstd,]
saveRDS(allstudies, paste0(pathTOfolder,"full_table_all_studies.rds"))

allstudies_noTaxon <- allstudies[is.na(allstudies$taxon_ids),] ## in case the people should be contacted
saveRDS(allstudies_noTaxon, paste0(pathTOfolder,"studies_w_no_taxon.rds"))

metadata_studies <- allstudies[,c(
  "id",
  "name",
  "taxon_ids",
  "number_of_individuals",
  "timestamp_first_deployed_location",
  "timestamp_last_deployed_location",
  "number_of_deployed_locations",
  "principal_investigator_name",
  "principal_investigator_email",
  "contact_person_name",
  "citation",
  "license_terms",
  "license_type",
  "access",
  # "xxxxx", ## attribute of resolution choice from limesurvey
  "download_date"
  )]

saveRDS(metadata_studies, paste0(pathTOfolder,"metadata_all_studies.rds"))

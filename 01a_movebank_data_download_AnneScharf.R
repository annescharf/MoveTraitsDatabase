
library(move2)

# specify account to use in the R session. 
options("move2_movebank_key_name" = "movebank") ## currently data are downloaded with the MB account "Anne.Hertel". Once we create a MB user just for this purpose, small changes will need to be done to this script.

# I would recommend to download and save each study separately, It makes debugging easier. And also putting all studies in one large object can be tricky as some of them can be enormously large
pathTOfolder <- "/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/CODE_DATABASE/MovebankData/"

all <- movebank_download_study_info() # some studies have years in weird formats, just ignore this warning message
all <- all[grep("GPS", all$sensor_type_ids),] # studies can have multiple sensors, making shure gps is included
all <- all[all$number_of_deployed_locations > units::set_units(0,"count"),] # removing those with 0 locations
all <- all[!is.na(all$number_of_deployed_locations),] # removing those with no deployed locations
all_open_gps_study <- all[which(all$license_type == "CC_0"),] 

## download all studies, and catching those where license agreement is needed
results <- lapply(all_open_gps_study$id, function(studyId)try({
  class(studyId) <- "integer64" ## lapply transforms the class into double for some reason
  mv2 <- movebank_download_study(studyId,
                                 sensor_type_id="gps",
                                 attributes = "individual_local_identifier")
  ## if one individual has several deployments, move2 automatically assignes each deployment to a different track. I guess in this case we want to work on a individual base, so here making sure that there is only one track per indiviudal, independant of the number of deployments
  if(mt_track_id_column(mv2)=="individual_local_identifier"){mv2 <- mv2}else{
    mv2 <- mt_set_track_id(mv2, "individual_local_identifier")}
  
  saveRDS(mv2, file=paste0(pathTOfolder,studyId,".rds"))
}))

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- all_open_gps_study$id
giveError <- all_open_gps_study$id[vapply(results, is.error, logical(1))]

## downloading those again that need license agreement, getting the 'license-md5' for each of the studies (it is different for each study).
results2 <- lapply(giveError, function(studyId)try({
  class(studyId) <- "integer64" ## lapply transforms the class into double for some reason
  mv2 <- movebank_download_study(studyId,
                                 sensor_type_id="gps",
                                 attributes = "individual_local_identifier",
                                 'license-md5'= sub('...Alternat.*','',sub('.*se-md5.=.','',
                                                                           as.character(rlang::catch_cnd(movebank_download_study(studyId))))))
  
  if(mt_track_id_column(mv2)=="individual_local_identifier"){mv2 <- mv2}else{
    mv2 <- mt_set_track_id(mv2, "individual_local_identifier")}
  
  saveRDS(mv2, file=paste0(pathTOfolder,studyId,".rds"))
}))

table(vapply(results2, is.error, logical(1)))
names(results2) <- giveError
results[vapply(results2, is.error, logical(1))]
giveError2 <- giveError[vapply(results, is.error, logical(1))]


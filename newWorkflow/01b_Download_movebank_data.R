# ---
# title: "MoveTraits Database"
# author: "Anne K. Scharf"
# date: "February 2025"
# ---

### in this script
## data is downloaded from movebank, each individual is downloaded separately. license agreements are accepted.
## studies and individuals are checked if they have changed since last download, only individuals that have more data get downloaded again

#######################################
## ToDo: make while loop to download all data when connection fails. See if it is possible to add a if file has not been saved for e.g. 1h, stop and start again. Sometimes code gets stuck.
## if studies will be saved from one month to he other and just live studies will be downloaded again, than think about what to do which studies that have been deleted from movebank in the mean time.
#######################################

library(move2)
library(bit64)
library(units)
library(R.utils)

# specify account to use in the R session
# keyring::key_list()
# movebank_store_credentials(username = "MoveTraits",
#                            password = "ehk7jwYDUJnRAlODazOvrp1B")
# 
# options("move2_movebank_key_name" = "MoveTraits")
options("move2_movebank_key_name" = "movebank")

pathTOfolder <- "./DATA/MoveTraitsData/"
dir.create(paste0(pathTOfolder,"1.MB_indv_mv2"))
pthDownld <- paste0(pathTOfolder,"1.MB_indv_mv2/")

### studies to download
allstudies <- readRDS(paste0(pathTOfolder,"full_table_all_studies.rds"))
Ids_toDo <- allstudies$id 

Ids_toDo <- LOUP

#### download by individual. object "result" only contains the error messages ######
start_time <- Sys.time()
# studyId <- Ids_toDo[1]
results <- lapply(Ids_toDo, function(studyId)try({
  ## create table with individuals per study, to be able to download per individual
  class(studyId) <- "integer64" ## lapply changes the class when looping though it
  print(studyId)
  
  ## if license terms have to be accepted, this is done here. These license terms are recorded in the metadata_table of script 1a
  reftb <-  tryCatch({
  movebank_download_deployment(study_id=studyId, omit_derived_data=F)
  }, error = function(e) {
    movebank_download_deployment(study_id=studyId, omit_derived_data=F,
                                        'license-md5'= sub('...Alternat.*','',sub('.*se-md5.=.','',as.character(rlang::catch_cnd(movebank_download_study(studyId))))))
  })

  reftb <- reftb[reftb$number_of_events > units::set_units(0,"count"),]
  
  ## intuitively one would use "individual_local_identifier" problem is: it soemtimes does not exist, names often contains symbols that mess with R like e.g. "/". The "individual_id" is an internal number not visible on the webpage, it is also consistent unless the study gets deleted and uploaded again. With around 20k individuals, this is the safest option
  reftb$pthName <- paste0(studyId,"_",reftb$individual_id,".rds")
  
  ## here it is checked which indiv have already been downloaded and which are missing
  doneIndv <- list.files(pthDownld)[grep(studyId,list.files(pthDownld))] 
  allStInd <- reftb$pthName ## individuals in study
  missInd <- allStInd[!allStInd%in%doneIndv] ## missing indiv
  print(paste0("done:",length(doneIndv),"-todo:",length(missInd)))
  todoIndv <-  reftb$individual_id[reftb$pthName%in% missInd] 
  
  ## download each individual separatly
  # ind <- todoIndv[2]
  results2 <- lapply(todoIndv, function(ind)try({
    class(ind) <- "integer64" ## lapply changes the class when looping though it
    print(paste0(studyId,"_",ind))
    mv2 <- movebank_download_study(studyId,
                                   sensor_type_id=c("gps"),                                                                           
                                   individual_id=ind,                                                                       
                                   attributes = c("individual_local_identifier","deployment_id"),
                                   timestamp_end=as.POSIXct(Sys.time(), tz="UTC")) # to avoid locations in the future

    saveRDS(mv2, file=paste0(pthDownld,studyId,"_",ind,".rds"))
  }))
}))
end_time <- Sys.time()
end_time-start_time # 

# saveRDS(results,paste0(saveErrorPath,"all",".rds"))

is.error <- function(x) inherits(x, "try-error")
table(vapply(results, is.error, logical(1)))
names(results) <- seq_along(results)
results[vapply(results, is.error, logical(1))]
# Check studies that returned errors:
giveError <- Ids_toDo[vapply(results, is.error, logical(1))]


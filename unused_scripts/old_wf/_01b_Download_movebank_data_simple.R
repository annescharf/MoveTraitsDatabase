
### in this script
## data is downloaded from movebank, each individual is downloaded separately. license agreements are accepted.
## studies and individuals are checked if they have changed since last download, only individuals that have more data get downloaded again

#######################################
## ToDo: make while loop to download all data when connection fails. See if it is possible to add a if file has not been saved for e.g. 1h, stop and start again. Sometimes code gets stuck.
#######################################

####################################################################
## SETTINGS: 
firstRound <- T # or F if data already have been downloaded
check4newData <- F # or T - next time download, only download indiv with new data
excludeDownlToday <- F # or T - if script broke while downloading, to not download all data that has been downloaded today already
## dates below not relevant for very first round
lastDwld <- "2025-02-06"
thisDwnl <- "2025-02-06" ## if interrupted
####################################################################

library(move2)
library(bit64)
library(units)
library(R.utils)

# specify account to use in the R session
# keyring::key_list()
options("move2_movebank_key_name" = "MoveTraits")

pathTOfolder <- "./MoveTraitsData/"
dir.create(paste0(pathTOfolder,"1.MB_indv_mv2"))
pthDownld <- paste0(pathTOfolder,"1.MB_indv_mv2/")

### studies to download
allstudies <- readRDS(paste0(pathTOfolder,"full_table_all_studies.rds"))

# #####
# if(!firstRound){
# ## when download is interrupted in the middle (internet connection in Bückle). Checking when files were last saved
# tb_pth <- data.frame(pth=list.files(pthDownld, full.names=T),filenm=list.files(pthDownld, full.names=F), mbid=sub("_.*", "", list.files(pthDownld)))
# tb_pth$lastSaved <- do.call(c,(lapply(tb_pth$pth, lastModified)))
# IDs_doneMdl <- unique(tb_pth$mbid[tb_pth$lastSaved >= as.POSIXct(thisDwnl)])
# 
# ### here studies are assigend the status "done" if there is no change in data since last download, and "live" if more data has been added since. "done" are ignored in the download
# IDs_done <- unique(sub("_.*", "", list.files(pthDownld)))
# compareDF <- allstudies
# compareDF$status <- NA
# compareDF$status[compareDF$id%in%IDs_done] <- "done"
# compareDF$status[compareDF$status=="done" & compareDF$timestamp_last_deployed_location>as.POSIXct(lastDwld, tz="UTC")] <- "live"
# table(compareDF$status)
# table(is.na(compareDF$status))
# }

Ids_toDo <- allstudies$id ## first round

# if(!firstRound){
# # and # subsequent rounds
# Ids_toDo <- compareDF$id[!compareDF$status%in%c("done")]
# # and ## interrupted in the middle
# Ids_toDo <- Ids_toDo[!Ids_toDo%in%IDs_doneMdl]
# }
# # studyId <- Ids_toDo[1]
# # studyId <-1764627
# 
# if(check4newData & excludeDownlToday){stop("Both 'check4newData' & 'excludeDownlToday' are set to TRUE, only one at a time can be set to TRUE, please choose one.")}
# if(excludeDownlToday){newerToday <- tb_pth$filenm[tb_pth$lastSaved >= as.POSIXct(thisDwnl)]} ## to exclude those downloaded today already
# if(check4newData){lastDownload <- tb_pth$filenm[tb_pth$lastSaved >= as.POSIXct(lastDwld)]} ## to download missed last time

########################
#### download by individual. object "result" only contains the error messages ######
start_time <- Sys.time()
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
  # indiv <- reftb$individual_local_identifier
  # if(any(grepl("/", indiv)==T)){indiv <- gsub("/","-",indiv)}
  # reftb$individual_local_identifierNObsl <- indiv
  
  ## intuitively one would use "individual_local_identifier" problem is: it soemtimes does not exist, names often contains symbols that mess with R like e.g. "/". The "individual_id" is an internal number not visible on the webpage, it is also consistent unless the study gets deleted and uploaded again. With around 20k individuals, this is the safest option
  reftb$pthName <- paste0(studyId,"_",reftb$individual_id,".rds")
  allindv <- unique(reftb$individual_id) 
  
  ## here already downloaded individuals are checked, and assigend with the status "done" if there is no change in data since last download, and "live" if more data has been added since. "done" are ignored in the download
  doneIndv <- list.files(pthDownld)[grep(studyId,list.files(pthDownld))] 
  # allStInd <- reftb$pthName ## individuals in study
  # missInd <- allStInd[!allStInd%in%doneIndv] ## missing indiv
  
  reftb$status <- NA
  reftb$status[reftb$pthName%in%doneIndv] <- "done"
  # reftb$status[reftb$status=="done" & reftb$timestamp_end_individual>as.POSIXct(lastDwld, tz="UTC")] <- "live"
  liveAndmissingIndv <- reftb$pthName[!reftb$status%in%c("done")]
  # if(excludeDownlToday){liveAndmissingIndv <- liveAndmissingIndv[!liveAndmissingIndv%in%newerToday]} # to exclude those downloaded today already
  # if(check4newData){liveAndmissingIndv <- liveAndmissingIndv[!liveAndmissingIndv%in%lastDownload]} # to download missed last download
  
  print(paste0("done:",length(doneIndv),"-todo:",length(liveAndmissingIndv)))
  
  # missIndstrp <- sub("^\\d+_(.+)\\.rds$", "\\1", liveAndmissingIndv)
  # ^\\d+_ matches the initial number and underscore
  # (.+) captures everything after that until the file extension
  # \\.rds$ matches the ".rds" at the end of the string
  # The \\1 in the replacement refers to the captured group, effectively keeping only the middle part of the string.
  
  todoIndv <-  reftb$individual_id[reftb$pthName%in% liveAndmissingIndv] 
                                                                                                                                                                                                                                                             
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


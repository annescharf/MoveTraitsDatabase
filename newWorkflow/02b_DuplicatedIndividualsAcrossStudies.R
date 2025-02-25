### in this script:
### duplicated data is identified. As the same data can be present partially 
## or entirely in multiple studies in MB, these need to be identified when 
## downloading all data of one or multiple species. Given that often the 
## individual_local_identifier is different for exactly the same data, we 
## rely heavily on tag id, as this is more likely to not change. Here we find 
## duplicated individuals/tags within/across studies:
# - check duplicated by tag-sps
# - check individuals with multiple tags simultaneously
# code checks if possible duplicated indivs overlap in tracking time, only if 
## they are overlaping in time they are marked as duplicated
# OPTION: keep the one with longest duration or with most gps points ~L115

#######################################
## ToDo: 
## future: merge metadata table with reference table to know opt in opt out of finde scaled traits
#######################################

##############################################################################
### SETTINGS ####
## choose which duplicate to keep, with longest duration ("duration") or with largest number of gps points ("locations")
keepCriterion <- "duration" # "duration", "locations"
##############################################################################

#######---------------------------------------------------------------------#########
## function: create reference table with 1 line per individualID-tagID combination ##
#######---------------------------------------------------------------------#########
library('move2')
library('lubridate')
library("data.table")

## get a reference table with 1 line per individualID-tagID combination
## So individuals which have multiple tags/deployments will have one line per tagID, 
## associated to the start and end of the tracking time for that specific tag.
# This is important for finding duplicated tagIDs within or across studies.

# path_to_indv_move2 <- "./MoveTraitsData/2.MB_indv_mv2_clean//404939825_426527460.rds"
referenceTableStudies <-  function(path_to_indv_move2){
  print(path_to_indv_move2) # this makes it easy to find which one gave an error
  indiv_mv2 <- readRDS(path_to_indv_move2)
  indiv_mv2_td <- mt_track_data(indiv_mv2)
  fileNameL <- strsplit(path_to_indv_move2,"/")
  fileName <- fileNameL[[1]][length(fileNameL[[1]])]
  
  # this is to account for when individual_local_identifier is missing
  if(all(is.na(indiv_mv2_td$individual_local_identifier))==T
     & all(is.na(indiv_mv2_td$tag_local_identifier))==F){ 
    indiv_mv2 <- mutate_track_data(indiv_mv2, individual_local_identifier = indiv_mv2_td$tag_local_identifier)
  }else if(all(is.na(indiv_mv2_td$individual_local_identifier))==T
           & all(is.na(indiv_mv2_td$tag_local_identifier))==T){warning("This study has neither individual nor tag identifier, study excluded from this step.")}
  # this is to account for when tag_local_identifier is missing
  if(all(is.null(indiv_mv2_td$tag_local_identifier))==T){ 
    indiv_mv2 <- mutate_track_data(indiv_mv2, tag_local_identifier=indiv_mv2_td$tag_id)
  }
  
  # this is to account for individuals with multiple tags (i.e deployments)
  tagList <- split(indiv_mv2, mt_track_id(indiv_mv2))
  
  indTable <- as.data.frame(rbindlist(lapply(tagList, function(tag){
    # tag <- tagList[[1]]
    tag_td <- mt_track_data(tag)
    rdf <- data.frame(
      fileName = fileName,
      MBid = tag_td$study_id,
      individual_local_identifier = as.character(tag_td$individual_local_identifier),
      tag_local_identifier = as.character(tag_td$tag_local_identifier),
      deployment_id = as.character(tag_td$deployment_id),
      # species = if(!is.null(tag_td$taxon_canonical_name)){tag_td$taxon_canonical_name}else{NA},
      species = if(!is.null(tag_td$taxon_canonical_name)){tag_td$taxon_canonical_name}else{tag_td$taxon_ids}, ## this leads to species having multiple species names in a string
      sex = if(!is.null(tag_td$sex)){tag_td$sex}else{NA}, 
      animal_mass = if(!is.null(tag_td$animal_mass)){tag_td$animal_mass}else{NA}, 
      animal_life_stage = if(is.null(tag_td$animal_life_stage)){NA}else if(is.na(unique(tag_td$animal_life_stage))){NA}else{unique(as.character(tag_td$animal_life_stage))}, ## included not for filtering here, but including it as needed afterwards
      manipulation_type = if(!is.null(tag_td$manipulation_type)){tag_td$manipulation_type}else{NA}, # 'none' or NA are not manipulated
      tracking_duration_days = round(as.numeric(difftime(range(mt_time(tag))[2],range(mt_time(tag))[1],units="days")),2),
      tracking_start_date = range(mt_time(tag))[1],  # Important to keep the full timestamp, as in the same day a tag can be removed from one individual and put on another one.
      tracking_end_date = range(mt_time(tag))[2],
      GPSpts_total = nrow(tag),
      median_timelag_mins = median(mt_time_lags(tag),na.rm=T),
      min_timelag_mins = min(mt_time_lags(tag),na.rm=T)
    )
    units(rdf$median_timelag_mins) <- units::make_units(minute)
    units(rdf$min_timelag_mins) <- units::make_units(minute)
    units(rdf$tracking_duration_days) <- units::make_units(day)
    units(rdf$animal_mass) <- units::make_units(g)
    rdf$median_timelag_mins <- round(rdf$median_timelag_mins)
    rdf$min_timelag_mins <- round(rdf$min_timelag_mins)
    return(rdf)
  })))
  return(indTable)
}
########### end function #################################

#######--------------------------#######
## create Individuals reference table ##
#######--------------------------#######
# Create a reference table, one entry per individual-tag combination: #####
pathTOfolder <- "./DATA/MoveTraitsData/"
pthClean <- paste0(pathTOfolder,"2.MB_indv_mv2_clean/")
flsMV <- list.files(pthClean, full.names = T)

start_time<- Sys.time()
referenceTableStudies_ALL <- as.data.frame(rbindlist(lapply(flsMV, referenceTableStudies)))
end_time <- Sys.time() 
end_time-start_time # 

saveRDS(referenceTableStudies_ALL, file=paste0(pathTOfolder,"/referenceTableStudies_ALL_original.rds")) ## just to making sure to have a copy that is untouched, as after this it will be modified and overwritten....  

#######--------------------------------#########
## filter reference table by certain criteria ##
#######--------------------------------#########

## ensure only keeping individuals with species name: ###
referenceTableStudies_ALL_nospsName <- referenceTableStudies_ALL[grep(",", referenceTableStudies_ALL$species),]
if(nrow(referenceTableStudies_ALL_nospsName)>0){saveRDS(referenceTableStudies_ALL_nospsName, file=paste0(pathTOfolder,"/individulas_no_taxon_name.rds"))} ## saving table incase feedback wants to be given
if(nrow(referenceTableStudies_ALL_nospsName)>0){
referenceTableStudies_ALL <- referenceTableStudies_ALL[!referenceTableStudies_ALL$fileName%in%referenceTableStudies_ALL_nospsName$fileName,]} ## removing those with multiple species names
referenceTableStudies_ALL <- referenceTableStudies_ALL[!is.na(referenceTableStudies_ALL$species),]

saveRDS(referenceTableStudies_ALL, file=paste0(pathTOfolder,"/referenceTableStudies_ALL_original_w_sps.rds")) ## just to making sure to have a copy that is untouched, as after this it will be modified and overwritten....  

## remove individuals according to their manipulation type
table(referenceTableStudies_ALL$manipulation_type)
referenceTableStudies_ALL <- referenceTableStudies_ALL[is.na(referenceTableStudies_ALL$manipulation_type) | referenceTableStudies_ALL$manipulation_type%in%"none",]
saveRDS(referenceTableStudies_ALL, file=paste0(pathTOfolder,"/referenceTableStudies_ALL_original_w_sps.rds"))

#######---------------------------------#########
## Find duplicated tags within/across studies: ##
#######---------------------------------#########
## tag names seem to be very consistent. Individual names change for the 
## same indiv that appear in several studies

library(DescTools) # for function %overlaps%
library(dplyr)
library(bit64)
referenceTableStudies_ALL <-  readRDS(paste0(pathTOfolder,"/referenceTableStudies_ALL_original_w_sps.rds"))
referenceTableStudies_ALL$MBid <- as.character(referenceTableStudies_ALL$MBid) ## class integer64 sometimes gets in loops, and converts to something else (very long number)

## ensure that max tracking end date corresponds to download date.
max(referenceTableStudies_ALL$tracking_end_date) 

## adding some columns for managing the table
referenceTableStudies_ALL$rowID <- paste0("rID_",1:nrow(referenceTableStudies_ALL))
referenceTableStudies_ALL$excluded <- "no"
referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps <- NA
referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps2 <- NA

## adding columns by which to filter
referenceTableStudies_ALL$Ind_Tag_Sps <- paste0(referenceTableStudies_ALL$individual_local_identifier,"_",referenceTableStudies_ALL$tag_local_identifier,"_",referenceTableStudies_ALL$species)
referenceTableStudies_ALL$Tag_Sps <- paste0(referenceTableStudies_ALL$tag_local_identifier,"_",referenceTableStudies_ALL$species)
referenceTableStudies_ALL$Stu_Ind_Sps <- paste0(referenceTableStudies_ALL$MBid,"_",referenceTableStudies_ALL$individual_local_identifier,"_",referenceTableStudies_ALL$species)

## check duplicated by tag-sps ----
# get table containing all rows of duplication
dupliTab_TS <- referenceTableStudies_ALL
dupliTab_TS <- dupliTab_TS[dupliTab_TS$Tag_Sps %in% dupliTab_TS$Tag_Sps[duplicated(dupliTab_TS$Tag_Sps)],] 

print(paste0(nrow(dupliTab_TS), " rows duplicated by tag and sps"))
if(nrow(dupliTab_TS)>1){
  # splitting to select which of the duplicateds to keep
  dupliTab_TS_l <- split(dupliTab_TS, dupliTab_TS$Tag_Sps)
  # toExclude_l <- lapply(dupliTab_TS_l, function(x){
  for(i in 1:length(dupliTab_TS_l)){
    print(i)
    x <- dupliTab_TS_l[[i]]
    
    # make matrix to find all paired combinations
    combiMatrix <- expand.grid(x$rowID,x$rowID,stringsAsFactors=F)
    colnames(combiMatrix) <- c("id1", "id2")
    combiMatrix <- combiMatrix[combiMatrix$id1!=combiMatrix$id2,] # removing comparisons to them selves (eg. ID2, ID2)
    combiMatrix <- combiMatrix[duplicated(t(apply(combiMatrix, 1, sort))),] # removing same comparison, but different order (eg. ID3-ID4 and ID4-ID3) as %overlaps% does not care about order
    combiMatrix$overlap <- FALSE
    # checking if overlap is true for all pairs
    for(i in 1:(nrow(combiMatrix))){
      if(c(x$tracking_start_date[x$rowID==combiMatrix$id1[i]], x$tracking_end_date[x$rowID==combiMatrix$id1[i]]) %overlaps% c(x$tracking_start_date[x$rowID==combiMatrix$id2[i]], x$tracking_end_date[x$rowID==combiMatrix$id2[i]]) == TRUE){
        combiMatrix[i,"overlap"] <- TRUE
      }
    }
    # not overlapping duplicated tag_sps in time: probably same(or different) study deployed tag on different individual
    overlapping <- combiMatrix[combiMatrix$overlap==TRUE,] 
    # accounting for table having multiple rows, e.g. 2 individuals got tagged with same tag, both are duplicated
    if(nrow(overlapping) > 0){
      ## keeping the one with the most gps pts
      if(keepCriterion=="locations"){
        print("keeping duplicate with largest number of locations")
        exclude_l <- lapply(1:(nrow(overlapping)),function(i){
          rowToExclude <- which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "GPSpts_total"])  # if same number of gps pts, 1st one gets declared as min
          x$rowID[x$rowID %in% overlapping[i,c("id1","id2")]][rowToExclude] # this works also when overlapping has multiple rows
        })
      }
      ## keeping the one with the longest duration
      if(keepCriterion=="duration"){
        print("keeping duplicate with longest duration")
        exclude_l <- lapply(1:(nrow(overlapping)),function(i){
          # if both tags have same tracking duration we take the one with more GPS locations, otherwise the one with longer duration
          if(x[x$rowID %in% overlapping[i,"id1"], "tracking_duration_days"] == x[x$rowID %in% overlapping[i,"id2"], "tracking_duration_days"]){
            x$rowID[which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "GPSpts_total"])]
          }else{x$rowID[which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "tracking_duration_days"])] # if same number of gps pts, 1st one gets declared as min
          }
        })
      }
      ## getting all duplicates together, sometimes its 2 and 2...
      exclude <- unique(unlist(exclude_l))
      # return(exclude)
      referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID %in% exclude] <- "yes_duplicated_Tag_Sps"
      ## indicating for the group of duplicated, which of them is kept, to afterwards better evaluate which are true duplicates
      all <- unique(c(overlapping$id1,overlapping$id2))
      keep <- all[!all%in%exclude]
      if(length(keep)==1){indKeep <- paste0(x$MBid[x$rowID==keep],"_", x$Ind_Tag_Sps[x$rowID==keep])
      print(indKeep)
      referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps2[referenceTableStudies_ALL$rowID %in% all] <- indKeep
      }else{
        for(j in 1:length(keep)){
          sel <- overlapping%>%filter_all(any_vars(. %in% c(keep[j])))
          indKeep <- paste0(x$MBid[x$rowID==keep[j]],"_", x$Ind_Tag_Sps[x$rowID==keep[j]])
          print(indKeep)
          referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps2[referenceTableStudies_ALL$rowID %in% c(sel[,1],sel[,2])] <- indKeep
        }                  
      }
    }
  }
}

## checking if a selected indiv got overwritten by another one
hck <- referenceTableStudies_ALL[, c("kept_MB_Ind_Tag_Sps","kept_MB_Ind_Tag_Sps2")]
# hck <- hck[rowSums(is.na(hck)) != ncol(hck), ]
hck <- hck[complete.cases(hck),]
identical(hck$kept_MB_Ind_Tag_Sps,hck$kept_MB_Ind_Tag_Sps2)

referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps[is.na(referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps)] <- referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps2[is.na(referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps)]
referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps2 <- NULL

saveRDS(referenceTableStudies_ALL, file=paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))

#######-----------------------------------------#########
## check individuals with multiple tags simultaneously ##
#######-----------------------------------------#########
# get table containing all rows of duplication
dupliTab_I <- referenceTableStudies_ALL[referenceTableStudies_ALL$excluded=="no",] ## removing those that have been already identified by duplicated Tag_Sps
dupliTab_I <- dupliTab_I[dupliTab_I$Stu_Ind_Sps %in% dupliTab_I$Stu_Ind_Sps[duplicated(dupliTab_I$Stu_Ind_Sps)],] 

print(paste0(nrow(dupliTab_I), " rows duplicated by individual"))
if(nrow(dupliTab_I)>1){
  # splitting to select which of the duplicateds to keep
  dupliTab_I_l <- split(dupliTab_I, dupliTab_I$Stu_Ind_Sps)
  toExclude_l <- lapply(dupliTab_I_l, function(x){
    # make matrix to find all paired combinations
    combiMatrix <- expand.grid(x$rowID,x$rowID,stringsAsFactors=F)
    colnames(combiMatrix) <- c("id1", "id2")
    combiMatrix <- combiMatrix[combiMatrix$id1!=combiMatrix$id2,] # removing comparisons to them selves (eg. ID2, ID2)
    combiMatrix <- combiMatrix[duplicated(t(apply(combiMatrix, 1, sort))),] # removing same comparison, but different order (eg. ID3-ID4 and ID4-ID3) as %overlaps% does not care about order
    combiMatrix$overlap <- FALSE
    # checking if overlap is true for all pairs
    for(i in 1:(nrow(combiMatrix))){
      if(c(x$tracking_start_date[x$rowID==combiMatrix$id1[i]], x$tracking_end_date[x$rowID==combiMatrix$id1[i]]) %overlaps% c(x$tracking_start_date[x$rowID==combiMatrix$id2[i]], x$tracking_end_date[x$rowID==combiMatrix$id2[i]]) == TRUE){
        combiMatrix[i,"overlap"] <- TRUE
      }
    }
    overlapping <- combiMatrix[combiMatrix$overlap==TRUE,] 
    if(nrow(overlapping) > 0){
      ## keeping the one with the most gps pts
      if(keepCriterion=="locations"){
        print("keeping duplicate with largest number of locations")
        exclude_l <- lapply(1:(nrow(overlapping)),function(i){
          rowToExclude <- which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "GPSpts_total"])  # if same number of gps pts, 1st one gets declared as min
          x$rowID[x$rowID %in% overlapping[i,c("id1","id2")]][rowToExclude] # this works also when overlapping has multiple rows
        })
      }
      ## keeping the one with the longest duration
      if(keepCriterion=="duration"){
        print("keeping duplicate with longest duration")
        exclude_l <- lapply(1:(nrow(overlapping)),function(i){
          # if both tags have same tracking duration we take the one with more GPS locations, otherwise the one with longer duration
          if(x[x$rowID %in% overlapping[i,"id1"], "tracking_duration_days"] == x[x$rowID %in% overlapping[i,"id2"], "tracking_duration_days"]){
            x$rowID[which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "GPSpts_total"])]
          }else{x$rowID[which.min(x[x$rowID %in% overlapping[i,c("id1","id2")], "tracking_duration_days"])] # if same number of gps pts, 1st one gets declared as min
          }
        })
      }
      exclude <- unlist(exclude_l)
      return(exclude)
    }
  })
  toExclude <- unlist(toExclude_l)
  referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID %in% toExclude] <- "yes_duplicated_Study-Individual-Species"
}

saveRDS(referenceTableStudies_ALL, file=paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))

table(referenceTableStudies_ALL$excluded)

#######-------------------------------------------------------------------------------#########
## ensuring all duplicates have been actually removed, and the correct one has been selected ##
#######-------------------------------------------------------------------------------#########
referenceTableStudies_ALL <- readRDS(file=paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))

checkDF <- referenceTableStudies_ALL[complete.cases(referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps),]

checkL <- split(checkDF,checkDF$kept_MB_Ind_Tag_Sps)
# table(unlist(lapply(checkL,nrow)))
# checkL[[100]]

## sometimes 2 or more indiv are kept 
table(unlist(lapply(checkL,function(x){
  nrow(x[x$excluded=="no",])
})))

## retaining duplicate with max "tracking_duration_days" or "GPSpts_total" depending on choice of "keepCriterion" at the top of the script
for(i in 1:length(checkL)){
  grp <- checkL[[i]]
    if(keepCriterion=="locations"){
    kp <- grp$rowID[grp$GPSpts_total==max(grp$GPSpts_total)]
    if(length(kp)>1){kp <- kp[1]}
    rids <- grp$rowID
    rids <- rids[!rids==kp]
    referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID%in%kp] <- "no"
    referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID%in%rids] <- "yes_duplicated"
  }  
  
  if(keepCriterion=="duration"){
    kp <- grp$rowID[grp$tracking_duration_days==max(grp$tracking_duration_days)]
    if(length(kp)>1){kp <- kp[1]}
    rids <- grp$rowID
    rids <- rids[!rids==kp]
    referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID%in%kp] <- "no"
    referenceTableStudies_ALL$excluded[referenceTableStudies_ALL$rowID%in%rids] <- "yes_duplicated"
  }
  # }
}
# sanity check
checkDF <- referenceTableStudies_ALL[complete.cases(referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps),]
checkL <- split(checkDF,checkDF$kept_MB_Ind_Tag_Sps)
table(unlist(lapply(checkL,function(x){
  nrow(x[x$excluded=="no",])
})))

saveRDS(referenceTableStudies_ALL, file=paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))

#######-------------------------------------------------#########
## Sanity checks: plots of duplicated individuals side by side ##
#######-------------------------------------------------#########
## Plot the duplicated tags to make sure it's the same individual. Tracks are 
## sampled to 1 position a day to make them lighter. Some plots may be empty 
## if tracks are shorter than one day

library(move2)
library(ggplot2)

pathTOfolder <- "./DATA/MoveTraitsData/"
pthClean <- paste0(pathTOfolder,"2.MB_indv_mv2_clean/")
dir.create(paste0(pathTOfolder,"3.duplicated_plots"))
plotPth <- paste0(pathTOfolder,"3.duplicated_plots/")

referenceTableStudies_ALL <- readRDS(file=paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))
checkDF <- referenceTableStudies_ALL[complete.cases(referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps),]
checkL <- split(checkDF,checkDF$kept_MB_Ind_Tag_Sps)

# y <- checkL[[2]]
lapply(checkL, function(y)try({
  print(y)
  df <- data.frame(fileName=y$fileName, keep=y$excluded)
  df$keep[!df$keep=="no"] <- "duplicate"
  df$keep[df$keep=="no"] <- "keep"
  
  dupL <- lapply(y$fileName, function(x){
    print(x)
    m <- readRDS(paste0(pthClean,x))
    m$fileName <- x
    m$fileName <- as.factor(m$fileName)
    m$keep <- df$keep[df$fileName==x]
    mt_track_id(m) <- "fileName"
    m <- mt_as_track_attribute(m,"individual_local_identifier")
    return(m)
  })
  mv2 <- mt_stack(dupL)
  mv2 <- mt_as_track_attribute(mv2,"keep",.keep=T)
  mv2 <- mt_filter_per_interval(mv2,criterion="first",unit="day") # making plots lighter
  jpeg(file=paste0(plotPth,mt_track_id(mv2)[1],".jpg"), width=20, height=12, units="cm", res=150)
  pl <- ggplot(data=mt_track_lines(mv2))+geom_sf(aes(color=keep))+facet_wrap(~fileName+individual_local_identifier,nrow=1)
  print(pl)
  dev.off()
}))



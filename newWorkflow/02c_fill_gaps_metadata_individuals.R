# ---
# title: "MoveTraits Database"
# author: "Anne K. Scharf"
# date: "February 2025"
# ---

## in this script:
## when one individual appears in several studies, than 'animal_life_stage', 'sex' & 'animal_mass' is looked up in all studies, because the selected study might contain NA, but the other studies might have values for these attrib. If the 'animal_life_stage' is different for different studies, a string ';' separated is created, as these differences can be due to wording. If 'sex' or 'animal_mass' have different values in different studies, NA is assigned as we cannot know which is the correct one.

#######
### ToDo:
## for the future if common name and locomotion mode need to be added, this might be a good place to do so.
#######

pathTOfolder <- "./MoveTraitsData/"
referenceTableStudies_ALL <- readRDS(file=paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))

table(is.na(referenceTableStudies_ALL$animal_life_stage))
table(referenceTableStudies_ALL$animal_life_stage)

dup_indv_L <- split(referenceTableStudies_ALL, referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps)

# i <- 1

filledoutL <- lapply(1:length(dup_indv_L), function(i){
  print(i)
  sub <- dup_indv_L[[i]]
  if(!all(is.na(sub$animal_life_stage))){
  sub$animal_life_stage[sub$excluded=="no"] <- paste0(unique(sub$animal_life_stage[!is.na(sub$animal_life_stage)]), collapse="; ")
  }
  if(!all(is.na(sub$sex))){ 
    sub$sex[sub$excluded=="no"] <- if(length(unique(sub$sex[!is.na(sub$sex)]))>1){NA}else{unique(sub$sex[!is.na(sub$sex)])}
  }
  if(!all(is.na(sub$animal_mass))){ 
    sub$animal_mass[sub$excluded=="no"] <- if(length(unique(sub$animal_mass[!is.na(sub$animal_mass)]))>1){NA}else{unique(sub$animal_mass[!is.na(sub$animal_mass)])}
  }
 
  if(any(is.na(sub$animal_life_stage))){
    if(!all(is.na(sub$animal_life_stage))){
      subna <- sub[is.na(sub$animal_life_stage),]
      subna$animal_life_stage <- paste0(unique(sub$animal_life_stage[!is.na(sub$animal_life_stage)]), collapse="; ")
      subret <- subna[,c("rowID","animal_life_stage")]
      return(subret)
    }
  }
})

filledoutDF <- do.call("rbind",filledoutL)
for(i in filledoutDF$rowID){
  referenceTableStudies_ALL$animal_life_stage[referenceTableStudies_ALL$rowID==i] <- filledoutDF$animal_life_stage[filledoutDF$rowID==i]
}

table(is.na(referenceTableStudies_ALL$animal_life_stage))
table(referenceTableStudies_ALL$animal_life_stage)

saveRDS(referenceTableStudies_ALL, file=paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn_more_metadata.rds"))
## use this table to merge all metadata with resulting traits. This table contains one row per individual-tag combination, ie, one individual can be split up among several lines if if had several tags. For merging use the columns: "study_id","individual_id","individual_local_identifier","tag_local_identifier". (Using ,"individual_local_identifier" and "individual_id" because individual_local_identifier is empty for some studies)


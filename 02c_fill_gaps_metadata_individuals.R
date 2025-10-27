# ---
# title: "MoveTraits Database"
# author: "Anne K. Scharf"
# date: "February 2025"
# ---

## in this script:
## when one individual appears in several studies, than 'animal_life_stage', 'sex', 'animal_mass', 'manipulation_type' is looked up in all studies, because the selected study might contain NA, but the other studies might have values for these attrib. If the 'animal_life_stage' is different for different studies, a string ';' separated is created, as these differences can be due to wording. If 'sex' or 'animal_mass' have different values in different studies, NA is assigned as we cannot know which is the correct one.

#######
### ToDo:
## for the future if common name and locomotion mode need to be added, this might be a good place to do so.
#######

pathTOfolder <- "./MoveTraitsData/"
referenceTableStudies_ALL <- readRDS(file=paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))

table(is.na(referenceTableStudies_ALL$animal_life_stage))
table(referenceTableStudies_ALL$animal_life_stage)
table(referenceTableStudies_ALL$manipulation_type)

## only select those individuals that are duplicated
dup_indv_L <- split(referenceTableStudies_ALL, referenceTableStudies_ALL$kept_MB_Ind_Tag_Sps) 

# i <- 1

for(i in 1:length(dup_indv_L)){
  print(i)
  sub <- dup_indv_L[[i]]
  if(!all(is.na(sub$manipulation_type))){
    referenceTableStudies_ALL$manipulation_type[referenceTableStudies_ALL$rowID==sub$rowID[sub$excluded=="no"]] <- paste0(unique(sub$manipulation_type[!is.na(sub$manipulation_type)]), collapse="; ")
  }
  if(!all(is.na(sub$animal_life_stage))){
    referenceTableStudies_ALL$animal_life_stage[referenceTableStudies_ALL$rowID==sub$rowID[sub$excluded=="no"]] <- paste0(unique(sub$animal_life_stage[!is.na(sub$animal_life_stage)]), collapse="; ")
  }
  if(!all(is.na(sub$sex))){ 
    referenceTableStudies_ALL$sex[referenceTableStudies_ALL$rowID==sub$rowID[sub$excluded=="no"]]  <- if(length(unique(sub$sex[!is.na(sub$sex)]))>1){NA}else{unique(sub$sex[!is.na(sub$sex)])}
  }
  if(!all(is.na(sub$animal_mass))){ 
    referenceTableStudies_ALL$animal_mass[referenceTableStudies_ALL$rowID==sub$rowID[sub$excluded=="no"]] <- if(length(unique(sub$animal_mass[!is.na(sub$animal_mass)]))>1){NA}else{unique(sub$animal_mass[!is.na(sub$animal_mass)])}
  }

}

move2::movebank_get_vocabulary("animal_mass")

table(referenceTableStudies_ALL$animal_life_stage[referenceTableStudies_ALL$excluded=="no"]) ## free text, NA
table(is.na(referenceTableStudies_ALL$animal_life_stage[referenceTableStudies_ALL$excluded=="no"]))

table(referenceTableStudies_ALL$animal_mass[referenceTableStudies_ALL$excluded=="no"]) ## in grams, NA
table(is.na(referenceTableStudies_ALL$animal_mass[referenceTableStudies_ALL$excluded=="no"]))

table(referenceTableStudies_ALL$sex[referenceTableStudies_ALL$excluded=="no"]) ## f, m, u, NA
table(is.na(referenceTableStudies_ALL$sex[referenceTableStudies_ALL$excluded=="no"]))

table(referenceTableStudies_ALL$manipulation_type[referenceTableStudies_ALL$excluded=="no"]) ## confined, domesticated, manipulated-other, none, reintroduction, relocated, NA
table(is.na(referenceTableStudies_ALL$manipulation_type[referenceTableStudies_ALL$excluded=="no"]))


saveRDS(referenceTableStudies_ALL, file=paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn_more_metadata.rds"))
## use this table to merge all metadata with resulting traits. This table contains one row per individual-tag combination, ie, one individual can be split up among several lines if if had several tags. For merging use the columns: "study_id","individual_id", "tag_local_identifier". 
## all entries with value "no" in the columns "excluded" are unique


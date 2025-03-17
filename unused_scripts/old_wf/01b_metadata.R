
library(move2)
library(tidyverse)

pathTOfolder <- "/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/CODE_DATABASE/MovebankData/"

list <- list.files("/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/CODE_DATABASE/MovebankData/",
                   pattern="*.rds")

sp.list <-
  data.frame(study_id = integer(),
             taxon_canonical_name = character(),
             taxon_ids =  character(),
             number_of_individuals = numeric(),
             timestamp_first_deployed_location = numeric(),
             timestamp_last_deployed_location = numeric(),
             number_of_deployed_locations = numeric(),
             citation = character(),
             principal_investigator_name = character(),
             principal_investigator_email = character(),
             contact_person_name =character(),
             filename = character())

for (i in 1:length(list))
{
      tr <- readRDS(paste0(pathTOfolder, list[i]))
  
  ## checking if there are columns in the track data that are a list. If yes, check if the content is the same, if yes remove list. If list columns are left over because content is different transform these into a character string (could be done as well as json, but think that average user will be more comfortable with text?)
  if(any(sapply(mt_track_data(tr), is_bare_list))){
    ## reduce all columns were entry is the same to one (so no list anymore)
    tr <- tr |> mutate_track_data(across(
      where( ~is_bare_list(.x) && all(purrr::map_lgl(.x, function(y) 1==length(unique(y)) ))), 
      ~do.call(vctrs::vec_c,purrr::map(.x, head,1))))
    if(any(sapply(mt_track_data(tr), is_bare_list))){
      ## transform those that are still a list into a character string
      tr <- tr |> mutate_track_data(across(
        where( ~is_bare_list(.x) && all(purrr::map_lgl(.x, function(y) 1!=length(unique(y)) ))), 
        ~unlist(purrr::map(.x, paste, collapse=","))))
    }
  }
  
  # filter duplicates
  tr <- tr %>% mt_filter_unique()
  tr.df <- mt_as_event_attribute(tr, names(mt_track_data(tr)))
  tr.df <- dplyr::mutate(tr.df, coords_x=sf::st_coordinates(tr.df)[,1], coords_y=sf::st_coordinates(tr.df)[,2])
  tr.df <- sf::st_drop_geometry(tr.df)
  movedata <- data.frame(tr.df)
  
  colVec <- c("study_id",
              "taxon_canonical_name",
              "taxon_ids",
              "number_of_individuals",
              "timestamp_first_deployed_location",
              "timestamp_last_deployed_location",
              "number_of_deployed_locations",
              "principal_investigator_name",
              "principal_investigator_email",
              "contact_person_name",
              "citation")

  movedata <- movedata[intersect(colVec, names(movedata))]
  movedata[setdiff(colVec, names(movedata))] <- NA
  movedata <- movedata[!duplicated(movedata$study_id),]
  
  sp.list <- rbind(sp.list,movedata)
  
}  
  
head(sp.list)
nrow(sp.list)

saveRDS(sp.list,"CODE_DATABASE/DATA/SpeciesList_movebank.rds")
write.csv(sp.list,"CODE_DATABASE/DATA/SpeciesList_movebank.csv", row.names=F)

# in a next step we manually extract each species common name based on
# the column taxon_canonical_name or taxon_ids
# we also manually asign a primary movement mode (fly, swim, walk)
# and whether species is a bird or mammal
# finally we include a column "include = yes / no", here we manually decide whether we 
# include a study or not. 
# we excluded studies from laboratory animals / in captivity  
# the filename to append common names is "SpeciesList_movebank_commonname.csv"



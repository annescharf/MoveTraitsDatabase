library(lubridate);library(metafor);library(tidyverse);library(amt);library(adehabitatLT);library(adehabitatHR); library(move2)

#study_list <- read.csv("CODE_DATABASE/DATA/SpeciesList_movebank.csv")

#study_list <- study_list %>% 
#  filter(include == "yes")

pathTOfolder <- "/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/CODE_DATABASE/MovebankData/"
pathTOfolder.out <- "/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/CODE_DATABASE/Movebankdata_df_v0.1/"

study <- list.files(pathTOfolder)
length(study)
#tr <- readRDS(paste0(pathTOfolder,"MovebankData2988333.rds"))

n <- length(study)
mylist <- vector("list", n)

for(i in 1:n) mylist[[i]] <- readRDS(paste0(pathTOfolder,study[[i]]))

for(i in 1:n) 
  {
  #i = 2
  tr <- mylist[[i]]  
  
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

colVec <- c("study_id","taxon_canonical_name","individual_id","timestamp","coords_x", "coords_y","animal_mass","sex","animal_life_stage")

movedata <- movedata[intersect(colVec, names(movedata))]
movedata[setdiff(colVec, names(movedata))] <- NA
movedata <-
  movedata %>% 
  mutate(
    OrdinalDay = lubridate::yday(timestamp),
    Year = lubridate::year(timestamp),
    Month = lubridate::month(timestamp),
    Hour = lubridate::hour(timestamp))

attr(movedata$timestamp, "tzone") <- "UTC"

saveRDS(movedata,paste0(pathTOfolder.out,study[[i]]))

}


### merge files to dataframes

# we are doing this in 3 chuncks to speed up processing and making it easier to find mistakes
# study xx did not flat into a dataframe format

files  <- list.files(path = pathTOfolder.out, pattern = '\\.rds')

# A
files2 <- lapply(paste0(pathTOfolder.out,files[1:50]), readRDS)
combined.df1 <- do.call(rbind , files2)
saveRDS(combined.df1,paste0(pathTOfolder.out,"combined.df1.rds"))

rm(files2)
rm(combined.df1)

# B - 51 : 100
files3 <- lapply(paste0(pathTOfolder.out,files[51:100]), readRDS)
combined.df2 <- do.call(rbind , files3)
saveRDS(combined.df2,paste0(pathTOfolder.out,"combined.df2.rds"))

rm(files3)
rm(combined.df2)

# C
files4 <- lapply(paste0(pathTOfolder.out,files[101:150]), readRDS)
combined.df3 <- do.call(rbind , files4)
saveRDS(combined.df3,paste0(pathTOfolder.out,"combined.df3.rds"))

rm(files4)
rm(combined.df3)

# D
files5 <- lapply(paste0(pathTOfolder.out,files[151:174]), readRDS)
combined.df4 <- do.call(rbind , files5)
saveRDS(combined.df4,paste0(pathTOfolder.out,"combined.df4.rds"))

rm(files5)
rm(combined.df4)

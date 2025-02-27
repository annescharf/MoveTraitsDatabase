# how many studies are missing?
library(bit64)

pathTOfolder <- "./DATA/MoveTraitsData/"
allstudies <- readRDS(paste0(pathTOfolder,"full_table_all_studies.rds"))
allstudies$id <- as.integer64(allstudies$id)
Ids_toDo <- allstudies$id 

length(Ids_toDo)

pathTOfolder <- "./DATA/MoveTraitsData/"
pthDownld <- paste0(pathTOfolder,"1.MB_indv_mv2/")
flsMV <- list.files(pthDownld, full.names = F)

# Load the stringr package
library(stringr)

# Split the string by both "_" and "." into exactly 3 pieces
result <- str_split_fixed(flsMV, "[_.]", 3)

# Convert the result into a data frame with three columns
result_df <- as.data.frame(result)
colnames(result_df) <- c("studyId", "indId", "Column3")

result_df$studyId <- as.integer64(result_df$studyId)

length(Ids_toDo) - length(unique(result_df$studyId))

library(Hmisc)
LOUP <- Ids_toDo[Ids_toDo %nin% unique(result_df$studyId)]

####

##
# Table S1. extract species in studies 
md <- readRDS(paste0(pathTOfolder,"/metadata_all_studies.rds"))

md <- 
  md |>
  dplyr::select(id,taxon_ids,license_type) |> 
  rename(study_id = id)

write.csv(md,"forTS1.csv")

## Prepare common name Lookup table 
x <- read.delim(pipe("pbpaste"))

x <- x[!duplicated(x$Species),]
clip <- pipe("pbcopy", "w")                       
write.table(x, file=clip)                               
close(clip)

metadata <- readRDS(paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))

library(tidyverse)
metadata <- 
  metadata |>
  distinct(species) |> 
  #  filter(excluded == "no") |>
  dplyr::select(species)

# filter species that are already in lookup table
metadata <- metadata[metadata$species %nin% x$Species,]

clip <- pipe("pbcopy", "w")                       
write.table(metadata, file=clip)                               
close(clip)

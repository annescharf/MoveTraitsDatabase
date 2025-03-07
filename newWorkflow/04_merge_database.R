# ---
# title: "MoveTraits Database"
# author: "Anne Hertel"
# date: "March 2025"
# ---

### in this script: 

# merge the databases created from movebank data and tucker data
# summarize the data at the species level
# save the database with repeated within-individual trait measures

## remove duplicates and outliers and studies without permission
## order columns in a logical way and rename if necessary


library(lubridate);library(metafor);library(tidyverse);library(amt);library(Hmisc)
library(adehabitatHR); library(move2); library(epitools); library(suncalc); library(purrr); library(bit64)

## ----Import movement data per individual-------------------------------------------------------------
pathTOfolder <- "./DATA/MoveTraitsData/"

#dir for individual summaries
pthtraitsum <- paste0(pathTOfolder,"5.MB_indv_traitsum/")

flsTS <- list.files(pthtraitsum, full.names = T)

# Read and combine all files while keeping all columns
db.movebank <- flsTS %>%
  lapply(readRDS) %>%        # Read each file into a list of data frames
  bind_rows()                 # Combine them into one data frame

# View the combined data
print(db.movebank)

dim(db.movebank)
colnames(db.movebank)

# remove empty column "n.max24h.days
nrow(db.movebank[!is.na(db.movebank$n.max24h.days),])
nrow(db.movebank[!is.na(db.movebank$n.dmax24h.days),])
db.movebank <- db.movebank[,c(1:16,83,18:82)]

# Remove rows with all NA values
db.movebank.1 <- 
  db.movebank %>%
  filter(!if_all(c("n1h":"di.05"), is.na))

## ----Merge individual level information data-------------------------------------------------------------

## Merge meta data and exclude duplicates and studies without permission
metadata <- readRDS(paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn_excludedStudies.rds"))

metadata <- 
  metadata |>
  mutate(individual_id = sapply(str_split(fileName, "_|\\."), function(x) x[2])) |>
  dplyr::select(MBid,individual_id,species,sex,animal_mass,
                animal_life_stage,manipulation_type,median_timelag_mins,
                tracking_duration_days, tracking_start_date, tracking_end_date,excluded) |> 
  rename(study_id = MBid) |> 
  mutate(study_individual = paste(study_id,individual_id,sep="_"))

metadata <- metadata |> 
  filter(!duplicated(study_individual)) |> 
  dplyr::select(study_individual,species,sex,animal_mass,
                animal_life_stage,median_timelag_mins,
                tracking_duration_days, tracking_start_date, tracking_end_date,excluded) 

#library(bit64)
#metadata$study_id <- as.integer64(metadata$study_id)

db.movebank.2 <- db.movebank.1 |>
  mutate(study_individual = paste(study_id,individual_id,sep="_")) |> 
  left_join(metadata, by = "study_individual") |> 
  filter(excluded == "no") |> 
  dplyr::select(-excluded)

## ----Merge study level information data-------------------------------------------------------------

metadata2 <- readRDS(paste0(pathTOfolder,"full_table_all_studies.rds"))
colnames(metadata2)[7] <- "study_id"

db.movebank.3 <- db.movebank.2 |>
  left_join(metadata2[,c("study_id","contact_person_name")], by = "study_id") 

#nrow(db.movebank.3) == nrow(db.movebank.2)  

## ----Merge common name and exclude reptiles, fish etc.-------------------------------------------------------------

commonname <- read.csv("/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/MoveTraitsDatabase_Git/MoveTraits_Git/DATA/SpeciesList_commonname.csv")
commonname <-
  commonname |> 
  rename("species" = "Species")

db.movebank.4 <- db.movebank.3 |> 
  left_join(commonname, by = "species") 

# species that were removed from database
table(db.movebank.4[db.movebank.4$include == "no",c("common_name")])

db.movebank.4 <- db.movebank.4 |> 
  filter(include == "yes") |> 
  dplyr::select(-include,-study_individual) |> 
  mutate(source = "movebank.mar2025")

#nrow(db.movebank.4) == nrow(db.movebank.3)  

# 5514 individuals
dim(db.movebank.4)
# 264 studies
length(unique(db.movebank.4$study_id))

## ----Merge Tucker data-------------------------------------------------------------

db.tucker <- readRDS("./DATA/Tucker/MoveTraitsDB.v0.1_Tucker.rds")

db.tucker <-
  db.tucker |> 
  filter(individual_id != 8) |> 
  filter(!duplicated(individual_id)) |> 
  mutate(animal_mass = animal_mass*1000) |>  # body mass in tucker in kg in movebank in grams
  mutate(source = "Tucker2023")

# Remove rows with all NA values
db.tucker <- 
  db.tucker %>%
  filter(!if_all(c("n1h":"di.05"), is.na))

# remove two studies that are duplicated in movebank! (see script 04b)
# This step needs to be automized in the future
db.tucker <- db.tucker |> 
  filter(species != "Connochaetes taurinus") |> 
  filter(!(species == "Cervus canadensis" & contact_person_name == "Mark Hebblewhite"))  

# remove czech red deer
db.tucker <- db.tucker |> 
  filter(contact_person_name != "Miloš Ježek") 

# all column names lower case  
names(db.tucker) <- tolower(names(db.tucker)) 

# add taxon - tucker
db.tucker <- db.tucker |> 
  left_join(commonname, by = "species") |> 
  dplyr::select(-include)

# dimensions of movebank and Tucker files
dim(db.tucker)
dim(db.movebank.4)

colnames(db.movebank.4)[colnames(db.movebank.4) %nin% colnames(db.tucker)]
colnames(db.tucker)[colnames(db.tucker) %nin% colnames(db.movebank.4)]

## bind database
MoveTrait.v0.1 <- plyr::rbind.fill(db.movebank.4,db.tucker)
dim(MoveTrait.v0.1)

## ----Save individual level Database-------------------------------------------------------------

# final recode of species labels 
MoveTrait.v0.1 <- MoveTrait.v0.1 |> 
  mutate(species = fct_recode(species, "Ovis canadensis" = "Ovis canadensis californiana")) |> 
  mutate(species = fct_recode(species, "Ovis canadensis" = "Ovis canadensis canadensis")) |> 
  mutate(species = fct_recode(species, "Ovis canadensis" = "Ovis canadensis nelsoni"))|> 
  mutate(species = fct_recode(species, "Loxodonta africana" = "Elephantidae"))|> 
  mutate(species = fct_recode(species, "Cervus elaphus" = "Cervus canadensis"))

# 119 bird sp., 61 mammal sp.
MoveTrait.v0.1 |> filter(!duplicated(species)) |> group_by(class) |>  tally()
MoveTrait.v0.1 |> filter(!duplicated(common_name)) |> group_by(class) |>  tally()
# 3853 bird ind., 3495 mammal ind. - 7348 ind total
MoveTrait.v0.1 |> tally()
MoveTrait.v0.1 |> group_by(class) |>  tally()
#1834 tucker, 5514 movebank
MoveTrait.v0.1 |> group_by(source) |>  tally()

dir.create(paste0(pathTOfolder,"7.MoveTraits_db"))
pthdb <- paste0(pathTOfolder,"7.MoveTraits_db/")
saveRDS(MoveTrait.v0.1, file=paste0(pthdb,"MoveTrait.v0.1_individual.sum_20250305.rds"))

## ----Species level Database-------------------------------------------------------------

## species summaries
  # Function to calculate the coefficient of variation
  cv <- function(x, na.rm = TRUE) {
    # Calculate standard deviation and mean
    sd_value <- sd(x, na.rm = na.rm)
    mean_value <- mean(x, na.rm = na.rm)
    cv_value <- sd_value / abs(mean_value)
    return(cv_value)
  }
  
MoveTrait.v0.1.sp <- MoveTrait.v0.1 |> 
  mutate(common_name = recode(common_name, "reindeer" = "reindeer/caribou")) |> 
  mutate(common_name = recode(common_name, "elk" = "red deer/elk")) |> 
  mutate(common_name = recode(common_name, "red deer" = "red deer/elk"))

MoveTrait.v0.1.species <-
  MoveTrait.v0.1.sp |> 
  group_by(species) |> 
  summarise(common_name = unique(common_name),
            class = unique(class),
            movement.mode = unique(movement.mode),
            across(c(6:10,12:16,18:22,24:28,30:34,36:40,42:46,48:52,54:58,60:64,66:70,72:76,78:82), 
                   mean, na.rm = TRUE),
            across(c(5,11,17,23,29,35,41,47,53,59,65,71,77), 
                             sum, na.rm = TRUE),
            contact_person_name = paste(unique(contact_person_name), collapse = ", ")) 

# sort columns
MoveTrait.v0.1.species <- MoveTrait.v0.1.species[,c(1:4,
                                                    70,5:9,
                                                    71,10:14,
                                                    72,15:19,
                                                    73,20:24,
                                                    74,25:29,
                                                    75,30:34,
                                                    76,35:39,
                                                    77,40:44,
                                                    78,45:49,
                                                    79,50:54,
                                                    80,55:59,
                                                    81,60:64,
                                                    82,65:69,
                                                    83)]

saveRDS(MoveTrait.v0.1.species, file=paste0(pthdb,"MoveTrait.v0.1_species.sum_20250305.rds"))

## ----Save within-individual level Database-------------------------------------------------------------

#dir for individual underlying traits
pthtrait <- paste0(pathTOfolder,"6.MB_indv_trait/")

flsTS <- list.files(pthtrait, full.names = T)

# Read and combine all files while keeping all columns
db.movebank <- flsTS %>%
  lapply(readRDS) %>%        
  bind_rows() 

# sort columns
db.movebank <- db.movebank[,c(1:16,85,18:82,86,90,87,93,94,83,84,91,95,88,92,96,89)]

# Remove rows with all NA values
db.movebank.1 <- 
  db.movebank %>%
  filter(!if_all(c("n1h":"di.05"), is.na))

## ----Merge individual level information data-------------------------------------------------------------

## Merge meta data and exclude duplicates
metadata <- readRDS(paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn_excludedStudies.rds"))

metadata <- 
  metadata |>
  mutate(individual_id = sapply(str_split(fileName, "_|\\."), function(x) x[2])) |>
  dplyr::select(MBid,individual_id,species,sex,animal_mass,
                animal_life_stage,manipulation_type,median_timelag_mins,
                tracking_duration_days, tracking_start_date, tracking_end_date,excluded) |> 
  rename(study_id = MBid) |> 
  mutate(study_individual = paste(study_id,individual_id,sep="_"))

metadata <- metadata |> 
  filter(!duplicated(study_individual)) |> 
  dplyr::select(study_individual,species,sex,animal_mass,
                animal_life_stage,median_timelag_mins,
                tracking_duration_days, tracking_start_date, tracking_end_date,excluded) 

db.movebank.2 <- db.movebank.1 |>
  mutate(study_individual = paste(study_id,individual_id,sep="_")) |> 
  left_join(metadata, by = "study_individual") |> 
  filter(excluded == "no") |> 
  dplyr::select(-excluded)

## ----Merge study level information data-------------------------------------------------------------

metadata2 <- readRDS(paste0(pathTOfolder,"full_table_all_studies.rds"))
colnames(metadata2)[7] <- "study_id"

db.movebank.3 <- db.movebank.2 |>
  left_join(metadata2[,c("study_id","contact_person_name","license_type","citation")], by = "study_id") 

## ----Merge common name and exclude reptiles, fish etc.-------------------------------------------------------------

commonname <- read.csv("/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/MoveTraitsDatabase_Git/MoveTraits_Git/DATA/SpeciesList_commonname.csv")
commonname <-
  commonname |> 
  rename("species" = "Species")

db.movebank.4 <- db.movebank.3 |> 
  left_join(commonname, by = "species") 

# species that were removed from database
table(db.movebank.4[db.movebank.4$include == "no",c("common_name")])

db.movebank.4 <- db.movebank.4 |> 
  filter(include == "yes") |> 
  dplyr::select(-include,-study_individual) |> 
  mutate(source = "movebank.mar2025")

#nrow(db.movebank.4) == nrow(db.movebank.3)  

# 5518 individuals
dim(db.movebank.4)
# 265 studies
length(unique(db.movebank.4$study_id))

## ----Merge Tucker data-------------------------------------------------------------

db.tucker <- readRDS("./DATA/Tucker/MoveTraitsDB.v0.1_spatial_Tucker.rds")

db.tucker <-
  db.tucker |> 
  filter(individual_id != 8) |> 
  mutate(animal_mass = animal_mass*1000) |> 
  mutate(source = "Tucker2023")

# remove duplicate wildebeest and elk
db.tucker <- db.tucker |> 
  filter(species != "Connochaetes taurinus") |> 
  filter(!(species == "Cervus canadensis" & contact_person_name == "Mark Hebblewhite"))  

# remove czech red deer
db.tucker <- db.tucker |> 
  filter(contact_person_name != "Miloš Ježek") 
  
names(db.tucker) <- tolower(names(db.tucker)) 
names(db.movebank.4) <- tolower(names(db.movebank.4)) 

# add taxon - tucker
db.tucker <- db.tucker |> 
  left_join(commonname, by = "species") |> 
  dplyr::select(-include)

# dimensions of movebank and Tucker files
dim(db.tucker)
dim(db.movebank.4)

colnames(db.movebank.4)[colnames(db.movebank.4) %nin% colnames(db.tucker)]
colnames(db.tucker)[colnames(db.tucker) %nin% colnames(db.movebank.4)]

## bind database
MoveTrait.v0.1.spatial <- plyr::rbind.fill(db.movebank.4,db.tucker)
dim(MoveTrait.v0.1.spatial)

MoveTrait.v0.1.spatial.2 <- MoveTrait.v0.1.spatial |> 
  dplyr::select("study_id","individual_id",
                "species","common_name","class","movement.mode",
                "sex","animal_mass","animal_life_stage","source",
                "mean.longitude":"diurnality","median_timelag_mins","tracking_duration_days",
                "tracking_start_date","tracking_end_date","contact_person_name",
                "license_type","citation")

## ----Save within-individual level Database-------------------------------------------------------------

# 3853 bird ind., 3495 mammal ind. - 7348 ind total
MoveTrait.v0.1.spatial.2 |> tally()
MoveTrait.v0.1.spatial.2 |> group_by(class) |>  tally()
MoveTrait.v0.1.spatial.2 |> group_by(source) |>  tally()

pthdb <- paste0(pathTOfolder,"7.MoveTraits_db/")
saveRDS(MoveTrait.v0.1.spatial.2, file=paste0(pthdb,"MoveTrait.v0.1_withinindividual_20250305.rds"))

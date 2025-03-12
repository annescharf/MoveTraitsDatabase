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
  left_join(metadata2[,c("study_id","contact_person_name","license_type","citation")], by = "study_id") 

## ----Rename species name-------------------------------------------------------------

#  Martes pennanti == Pekania pennanti
db.movebank.3[db.movebank.3$species %in% c("Pekania pennanti"),"species"] <-"Martes pennanti"

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

# 4574 individuals
dim(db.movebank.4)
# 243 studies
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

MoveTrait.v0.1 <- MoveTrait.v0.1 |> 
  dplyr::select("study_id","individual_id",
                "species","common_name","class","movement.mode",
                "sex","animal_mass","animal_life_stage","source",
                "mean.longitude":"di.05","median_timelag_mins","tracking_duration_days",
                "tracking_start_date","tracking_end_date","contact_person_name",
                "license_type","citation")

## ----Save individual level Database-------------------------------------------------------------

# final recode of species labels 
MoveTrait.v0.1 <- MoveTrait.v0.1 |> 
  mutate(species = fct_recode(species, "Ovis canadensis" = "Ovis canadensis californiana")) |> 
  mutate(species = fct_recode(species, "Ovis canadensis" = "Ovis canadensis canadensis")) |> 
  mutate(species = fct_recode(species, "Ovis canadensis" = "Ovis canadensis nelsoni"))|> 
  mutate(species = fct_recode(species, "Loxodonta africana" = "Elephantidae"))|> 
  mutate(species = fct_recode(species, "Cervus elaphus" = "Cervus canadensis"))

# 108 bird sp., 55 mammal sp.
MoveTrait.v0.1 |> filter(!duplicated(species)) |> group_by(class) |>  tally()
# 3660 bird ind., 2691 mammal ind. - 6351 ind total
MoveTrait.v0.1 |> tally()
MoveTrait.v0.1 |> group_by(class) |>  tally()
#1777 tucker, 4574 movebank
MoveTrait.v0.1 |> group_by(source) |>  tally()

dir.create(paste0(pathTOfolder,"7.MoveTraits_db"))
pthdb <- paste0(pathTOfolder,"7.MoveTraits_db/")
saveRDS(MoveTrait.v0.1, file=paste0(pthdb,"MoveTrait.v0.1_individual.sum_20250311.rds"))

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

MoveTrait.v0.1.sp <-
  MoveTrait.v0.1.sp |> 
  dplyr::select(3:6,13:90,95) 

MoveTrait.v0.1.sp2 <- 
  MoveTrait.v0.1.sp |> 
  group_by(species) |>
  mutate(species = unique(species),
            common_name = unique(common_name),
            class = unique(class),
            movement.mode = unique(movement.mode),
            across(c("n1h","n24h.days","n.dmax24h.days","n.dmax7d.weeks",
                            "n.max12m.years","n.mcp24h.days","n.mcp7d.weeks",  "n.mcp1m.months",
                            "n.mcp12m.years","n.iou24h.days","n.iou1m.month", "n.iou12m.year", 
                            "n.di.days"), 
                          sum, na.rm = TRUE),
            across(c("d1h.mean",       "d1h.median",     "d1h.cv",         "d1h.95",        
                            "d1h.05",         "d24h.mean",      "d24h.median",    "d24h.cv",       
                            "d24h.95",        "d24h.05",        "dmax24h.mean",   "dmax24h.median",
                            "dmax24h.cv",     "dmax24h.95",     "dmax24h.05",     "dmax7d.mean",   
                            "dmax7d.median",  "dmax7d.cv",      "dmax7d.95",      "dmax7d.05",     
                            "dmax12m.mean",   "dmax12m.median", "dmax12m.cv",     "dmax12m.95",    
                            "dmax12m.05",     "mcp24h.mean",    "mcp24h.median",  "mcp24h.cv",     
                            "mcp24h.95",      "mcp24h.05",      "mcp7d.mean",     "mcp7d.median",  
                            "mcp7d.cv",       "mcp7d.95",       "mcp7d.05",       "mcp1m.mean",    
                            "mcp1m.median",   "mcp1m.cv",       "mcp1m.95",       "mcp1m.05",      
                            "mcp12m.mean",    "mcp12m.median",  "mcp12m.cv",      "mcp12m.95",     
                            "mcp12m.05",      "iou24h.mean",    "iou24h.median",  "iou24h.cv",     
                            "iou24h.95",      "iou24h.05",      "iou1m.mean",     "iou1m.median",  
                            "iou1m.cv",       "iou1m.95",       "iou1m.05",       "iou12m.mean",   
                            "iou12m.median",  "iou12m.cv",      "iou12m.95",      "iou12m.05",     
                            "di.mean",        "di.median",      "di.cv",          "di.95",         
                            "di.05"), 
                   mean, na.rm = TRUE),
            contact_person_name = paste(unique(contact_person_name), collapse = ", ")) |> 
  distinct()


saveRDS(MoveTrait.v0.1.sp2, file=paste0(pthdb,"MoveTrait.v0.1_species.sum_20250311.rds"))

## ----Save within-individual level Database-------------------------------------------------------------

#dir for individual underlying traits
pthtrait <- paste0(pathTOfolder,"6.MB_indv_trait/")

flsTS <- list.files(pthtrait, full.names = T)

# Read and combine all files while keeping all columns
db.movebank <- flsTS %>%
  lapply(readRDS) %>%        
  bind_rows() 

# sort columns
db.movebank <- db.movebank[,c(1:82,85,89,86,92,93,83,84,90,87,91,94,95,88)]

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

## ----Rename species name-------------------------------------------------------------

#  Martes pennanti == Pekania pennanti
db.movebank.3[db.movebank.3$species %in% c("Pekania pennanti"),"species"] <-"Martes pennanti"

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

# 4560 individuals
dim(db.movebank.4)
# 243 studies
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

db.tucker |> group_by(class) |>  tally()

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
saveRDS(MoveTrait.v0.1.spatial.2, file=paste0(pthdb,"MoveTrait.v0.1_withinindividual_20250311.rds"))

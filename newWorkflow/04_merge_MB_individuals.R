
library(lubridate);library(metafor);library(tidyverse);library(amt);
library(adehabitatHR); library(move2); library(epitools); library(suncalc); library(purrr); library(bit64)

## ----Import movement data per individual-------------------------------------------------------------
pathTOfolder <- "./DATA/MoveTraitsData/"

#dir for individual summaries
pthtraitsum <- paste0(pathTOfolder,"5.MB_indv_traitsum/")
#dir for individual underlying traits
pthtrait <- paste0(pathTOfolder,"6.MB_indv_trait/")

flsTS <- list.files(pthtraitsum, full.names = T)

# Read and combine all files while keeping all columns
combined_data <- flsTS %>%
  lapply(readRDS) %>%        # Read each file into a list of data frames
  bind_rows()                 # Combine them into one data frame

# View the combined data
print(combined_data)

dim(combined_data)
head(data.frame(combined_data))

# Remove rows with all NA values
combined_data_cleaned <- 
  combined_data %>%
  filter(!if_all(c("n1h":"n.dmax7d.weeks"), is.na))

# 3012 individuals
# 4588 individuals
dim(combined_data_cleaned)
# 148 studies
# 194 studies
length(unique(combined_data_cleaned$study_id))

head(data.frame(combined_data_cleaned))
table(combined_data_cleaned$study_id)

combined_data_cleaned <- combined_data_cleaned[,c(1:22,83,24:82)]

# Original:
# 92 bird and 47 land-mammal species
# represented by 6220 individuals (3748 birds, 2472 mammals)

metadata <- readRDS(paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))

metadata <- 
  metadata |>
  mutate(individual_id = sapply(str_split(fileName, "_|\\."), function(x) x[2])) |>
  filter(excluded == "no") |>
  dplyr::select(MBid,individual_id,species,sex,animal_mass,
                animal_life_stage,manipulation_type,median_timelag_mins,
                tracking_duration_days, tracking_start_date, tracking_end_date) |> 
  rename(study_id = MBid)

library(bit64)
metadata$study_id <- as.integer64(metadata$study_id)


combined_data_cleaned <- combined_data_cleaned |> 
  left_join(metadata, by = "individual_id")




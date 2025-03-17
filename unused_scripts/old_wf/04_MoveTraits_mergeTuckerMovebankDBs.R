library(tidyverse);library(Hmisc)

db.movebank1 <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTraitsDB.v0.1_movebankI.rds")
db.movebank2 <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTraitsDB.v0.1_movebankII.rds")
db.movebank3 <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTraitsDB.v0.1_movebankIII.rds")
db.movebank4 <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTraitsDB.v0.1_movebankIV.rds")

db.movebank <- rbind(db.movebank1,db.movebank2,db.movebank3,db.movebank4)

# Metadata included thus far
# "study_id"         "ID"               "Species"         
# "BodyMass_g"       "Sex"              "Lifestage" 

# Tracks without Species identifier
species.na <- db.movebank[is.na(db.movebank$Species),c("study_id","ID","Species")]
unique(species.na$study_id)


# add common name, contact person etc - movebank
library(bit64)
citation_list.m <- readRDS("CODE_DATABASE/DATA/SpeciesList_movebank.rds")
study_list.m <- read.csv("CODE_DATABASE/DATA/SpeciesList_movebank_common name.csv")
study_list.m$study_id <- as.integer64(study_list.m$study_id)
study_list.m <- study_list.m[,1:6]

study_list.m <- study_list.m |> left_join(citation_list.m[,c(1,5,6,8:11)], by = "study_id") 

# recover missing species from metadata 

# we remove one study becasue it conatains tracks from multiple species but unclear which
# tarck belongs to which species
db.movebank <- db.movebank[db.movebank$study_id %nin% "1531481854",]
missing.species <- study_list.m[study_list.m$study_id %nin% "1531481854" & 
                                  study_list.m$study_id %in% species.na$study_id,c("study_id","Species")]  

db.movebank2 <- db.movebank %>%
  left_join(missing.species, by = c("study_id")) %>%
  mutate(Species.x = as.character(Species.x)) |> 
  mutate(Species = ifelse(is.na(Species.x), Species.y, Species.x)) |> 
  dplyr::select(-c(Species.y, Species.x)) |> 
  mutate(Species = factor(Species)) 

# merge contact person & citation to database

db.movebank3 <- db.movebank2 |> left_join(study_list.m, by = c("study_id","Species")) |> 
  mutate(source = "Movebank.06.2024")

db.movebank3 <- db.movebank3[,c(1,2,98:101,3:5,103:104,6:97,105:109,102)]

db.movebank4 <- db.movebank3 |> 
  filter(include == "yes") |> 
  dplyr::select(-include)

# Marlee Tucker data

db.tucker <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTraitsDB.v0.1_Tucker.rds")

db.tucker <-
db.tucker |> 
  mutate(BodyMass_kg = BodyMass_kg*1000) |> 
  rename("BodyMass_g" = "BodyMass_kg") |> 
  rename("contact_person_name" = "ContactPerson") |> 
  mutate(source = "Tucker2023")
  
# add taxon - tucker
study_list.t <- read.csv("CODE_DATABASE/DATA/SpeciesList_tucker.csv")
db.tucker <- db.tucker |> left_join(study_list.t, by = "Species")

# dimensions of movebank and Tucker files
dim(db.tucker)
dim(db.movebank4)

## bind database
MoveTrait.v0.1 <- plyr::rbind.fill(db.movebank4,db.tucker)
dim(MoveTrait.v0.1)

saveRDS(MoveTrait.v0.1,"MoveTraits.V0.1/MoveTraits.v0.1.rds") 
write.csv(MoveTrait.v0.1,"MoveTraits.V0.1/MoveTraits.v0.1.csv",row.names=F)

################################################################################
### INTRAINDIVIDUAL - SPATIAL ###
################################################################################

db.tucker.sp <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTraitsDB.v0.1_spatial_Tucker.rds")
db.movebank1.sp <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTrait.v0.1_spatial_movebankI.rds")
db.movebank2.sp <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTrait.v0.1_spatial_movebankII.rds")
db.movebank3.sp <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTrait.v0.1_spatial_movebankIII.rds")
db.movebank4.sp <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTrait.v0.1_spatial_movebankIV.rds")

db.movebank.sp <- rbind(db.movebank1.sp,db.movebank2.sp,db.movebank3.sp,db.movebank4.sp)

# add misisng species
db.movebank.sp2 <- db.movebank.sp %>%
  filter(study_id %nin% "1531481854") |> 
  left_join(missing.species, by = c("study_id")) %>%
  mutate(Species.x = as.character(Species.x)) |> 
  mutate(Species = ifelse(is.na(Species.x), Species.y, Species.x)) |> 
  dplyr::select(-c(Species.y, Species.x)) |> 
  mutate(Species = factor(Species)) |> 
  left_join(study_list.m, by = c("study_id","Species")) |> 
  mutate(source = "Movebank.06.2024") |> 
  filter(include == "yes") |> 
  dplyr::select(-include)
  


##
db.tucker.sp2 <-
  db.tucker.sp |> 
  mutate(BodyMass_kg = BodyMass_kg*1000) |> 
  rename("BodyMass_g" = "BodyMass_kg") |> 
  rename("contact_person_name" = "ContactPerson") |> 
  mutate(source = "Tucker2023") |> 
  left_join(study_list.t, by = "Species")


# Remove columns "ID_Day","ID_month","ID_year" from each nested data frame of Intensity of Use
db.movebank.sp3 <- 
  db.movebank.sp2 %>%
  mutate(IoU.24h = map(IoU.24h, ~ {
    if (is.null(.x)) {
      # If the data is NULL, return it as is
      NULL
    } else if (inherits(.x, "grouped_df")) {
      # If the data is a grouped_df, ungroup and remove column 'z'
      .x %>% ungroup() %>% dplyr::select(-ID_Day)
    } else {
      # If it's neither NULL nor grouped_df (unlikely here), return as is
      .x
    }
  }))
  
db.movebank.sp3 <- 
  db.movebank.sp3 %>%
  mutate(IoU.1m = map(IoU.1m, ~ {
    if (is.null(.x)) {
      # If the data is NULL, return it as is
      NULL
    } else if (inherits(.x, "grouped_df")) {
      # If the data is a grouped_df, ungroup and remove column 'z'
      .x %>% ungroup() %>% dplyr::select(-ID_Month)
    } else {
      # If it's neither NULL nor grouped_df (unlikely here), return as is
      .x
    }
  }))


db.movebank.sp3 <- 
  db.movebank.sp3 %>%
  mutate(IoU.12m = map(IoU.12m, ~ {
    if (is.null(.x)) {
      # If the data is NULL, return it as is
      NULL
    } else if (inherits(.x, "grouped_df")) {
      # If the data is a grouped_df, ungroup and remove column 'z'
      .x %>% ungroup() %>% dplyr::select(-ID_year)
    } else {
      # If it's neither NULL nor grouped_df (unlikely here), return as is
      .x
    }
  }))


## bind database
MoveTrait.v0.1.sp <- plyr::rbind.fill(db.movebank.sp3,db.tucker.sp2)
dim(MoveTrait.v0.1.sp)

# dimensions of movebank and Tucker files
dim(db.tucker.sp2)
dim(db.movebank.sp3)

saveRDS(MoveTrait.v0.1.sp,"MoveTraits.V0.1/MoveTraits.v0.1_spatial.rds") 




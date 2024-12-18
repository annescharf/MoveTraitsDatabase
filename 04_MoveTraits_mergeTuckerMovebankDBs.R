library(tidyverse)

db.tucker <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTraitsDB.v0_Tucker.rds")
db.movebank1 <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTraitsDB.v0.1_movebankI.rds")
db.movebank2 <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTraitsDB.v0.1_movebankII.rds")
db.movebank3 <- readRDS("CODE_DATABASE/Database_Tucker_MovebankV0.1/MoveTraitsDB.v0.1_movebankIII.rds")

db.movebank1 <-
  db.movebank1 %>%
  mutate(BodyMass_g = map_dbl(BodyMass_g, first),
         Lifestage = map_chr(Lifestage, first))

db.movebank2 <-
  db.movebank2 %>%
  mutate(BodyMass_g = map_chr(BodyMass_g, first),
         Lifestage = map_chr(Lifestage, first)) 
db.movebank2$BodyMass_g <- as.numeric(db.movebank2$BodyMass_g)

db.movebank3 <-
  db.movebank3 %>%
  mutate(BodyMass_g = map_dbl(BodyMass_g, first),
         Lifestage = map_chr(Lifestage, first))

db.movebank <- rbind(db.movebank1,db.movebank2,db.movebank3)

# dimensions of movebank and Tucker files
dim(db.tucker)
dim(db.movebank)

db.tucker <-
db.tucker |> 
  mutate(BodyMass_kg = BodyMass_kg*1000) |> 
  rename("BodyMass_g" = "BodyMass_kg") |> 
  mutate(Sex = NA,
         Lifestage = NA,
         source = "Tucker2023")
  
# add taxon - tucker
study_list.t <- read.csv("DATA/SpeciesList_tucker.csv")
db.tucker <- db.tucker |> left_join(study_list.t, by = "Species")

# add common name, contact perosn etc - movebank
library(bit64)
citation_list.m <- readRDS("CODE_DATABASE/DATA/SpeciesList_movebank.rds")
study_list.m <- read.csv("CODE_DATABASE/DATA/SpeciesList_movebank_common name.csv")
study_list.m$study_id <- as.integer64(study_list.m$study_id)
study_list.m <- study_list.m[,1:6]

study_list.m <- study_list.m |> left_join(citation_list.m[,c(1,5,6,8:11)], by = "study_id") 

db.movebank <- db.movebank |> left_join(study_list.m, by = c("study_id","Species")) |> 
  mutate(source = "Movebank.06.2024")

#colnames(db.movebank)["contact_person_name"] <- "ContactPerson"

## bind database
MoveTrait.v0 <- rbind(db.tucker,db.movebank)

saveRDS(MoveTrait.v0.1,"MoveTraits.v0.1.rds") 
write.csv(MoveTrait.v0.1,"MoveTraits.v0.1.csv",row.names=F)

################################################################################
### SPATIAL ###
################################################################################

db.tucker <- readRDS("Database_Tucker_Movebank/MoveTrait.v0_spatial_Tucker.rds")
db.movebank1 <- readRDS("Database_Tucker_Movebank/MoveTrait.v0_spatial_movebankI.rds")
db.movebank2 <- readRDS("Database_Tucker_Movebank/MoveTrait.v0_spatial_movebankII.rds")
db.movebank3 <- readRDS("Database_Tucker_Movebank/MoveTrait.v0_spatial_movebankIII.rds")

db.movebank1 <-
  db.movebank1 %>%
  mutate(BodyMass_g = map_dbl(BodyMass_g, first),
         Lifestage = map_chr(Lifestage, first))

db.movebank2 <-
  db.movebank2 %>%
  mutate(BodyMass_g = map_chr(BodyMass_g, first),
         Lifestage = map_chr(Lifestage, first)) 
db.movebank2$BodyMass_g <- as.numeric(db.movebank2$BodyMass_g)

db.movebank3 <-
  db.movebank3 %>%
  mutate(BodyMass_g = map_dbl(BodyMass_g, first),
         Lifestage = map_chr(Lifestage, first))

db.movebank <- rbind(db.movebank1,db.movebank2,db.movebank3)
# fix in next iteration!!!
db.movebank$ContactPerson <- NA

##
db.tucker <-
  db.tucker |> 
  mutate(BodyMass_kg = BodyMass_kg*1000) |> 
  rename("BodyMass_g" = "BodyMass_kg") |> 
  mutate(Sex = NA,
         Lifestage = NA,
         source = "Tucker2023")

# add taxon - tucker
study_list.t <- read.csv("DATA/SpeciesList_tucker.csv")
db.tucker <- db.tucker |> left_join(study_list.t, by = "Species")

# add taxon - movebank
study_list.m <- read.csv("DATA/SpeciesList_movebank.csv")
db.movebank <- db.movebank |> left_join(study_list.m, by = "Species") |> 
  mutate(source = "Movebank.06.2024")

## bind database
MoveTrait.v0 <- rbind(db.tucker,db.movebank)

# dimensions of movebank and Tucker files
dim(db.tucker)
dim(db.movebank)

## bind database
MoveTrait.v0 <- rbind(db.tucker,db.movebank)

saveRDS(MoveTrait.v0,"MoveTraits.v0_spatial.rds") 



#### Add Reindeer
MoveTrait.v0 <- readRDS("MoveTraits.v0_spatial.rds") 
Move.reindeer <- readRDS("Database_Tucker_Movebank/MoveTrait.v0_spatial_movebank_SvalbardReindeer.rds")  

Move.reindeer <-
  Move.reindeer %>%
  mutate(BodyMass_g = map_dbl(BodyMass_g, first),
         Lifestage = map_chr(Lifestage, first))
Move.reindeer$ContactPerson <- NA

study_list.m <- read.csv("DATA/SpeciesList_movebank.csv")
Move.reindeer <- Move.reindeer |> left_join(study_list.m, by = "Species") |> 
  mutate(source = "Movebank.06.2024")

MoveTrait.v0.1 <- rbind(MoveTrait.v0,Move.reindeer)
nrow(MoveTrait.v0.1) == (nrow(MoveTrait.v0) + nrow(Move.reindeer))
saveRDS(MoveTrait.v0.1,"MoveTraits.v0.1_spatial.rds") 

# ---
# title: "MoveTraits Database - Problem"
# author: "Anne G. Hertel"
# date: "28/1/2025"
# ---

library(lubridate);library(metafor);library(tidyverse);library(amt);
library(adehabitatHR); library(move2); library(epitools); library(suncalc); library(purrr);library(bit64)

## ----Structure of existing database-------------------------------------------------------------
db.V1 <- readRDS("/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/MoveTraits.V0.1/MoveTraits.v0.1.rds")
db.V1.sp <- readRDS("/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/MoveTraits.V0.1/MoveTraits.v0.1_spatial.rds")

# second individual has no annual estimates 
db.V1[db.V1$ID %in% c("2190902582","2298755403"),]

# second individual has "NULL" for intra-individual estimates of annual metric, while the first
# has a tibble
str(db.V1.sp[db.V1.sp$ID %in% c("2190902582","2298755403"),c("Dmax12m.mean","MaxDispl.12m")])

# when we unnest at the intra individual level we only get estimates for the one ID that has annual estimates 
db.V1.sp |> 
  filter(ID %in% c("2190902582","2298755403")) |> 
  unnest(MaxDispl.12m) |> 
  dplyr::select(ID,Species,common_name,year,Dmax12m,mean.x, mean.y)



# REDOE DB AT INDIVIDUAL LEVEL RETAINING ORGINAL STRUCTURE

## ----Import movement data per individual-------------------------------------------------------------
pathTOfolder <- "/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/CODE_DATABASE/Movebankdata_df_v0.1/"
animlocs.1hourly <- readRDS(paste0(pathTOfolder,"try",".rds"))
#animlocs.1hourly <- readRDS(paste0(pathTOfolder,"try_2298755403",".rds"))

## ----Resample data-------------------------------------------------------------
# Resample data to 24h, 7 week time scales using amt
animlocs.daily <- animlocs.1hourly |> 
  track_resample(rate = hours(24),
               tolerance = minutes(60)) 

animlocs.weekly <- animlocs.1hourly |> 
  track_resample(rate = hours(24*7),
                 tolerance = minutes(60*24)) 


data_resampled <- animlocs.1hourly %>%
  tidyr::nest(data = -individual_id) %>% 
  dplyr::select(-data) %>% # just a quick trick to keep things flowing.
  left_join(., animlocs.1hourly[!duplicated(animlocs.1hourly$individual_id),
                        c("taxon_canonical_name","individual_id","animal_mass","sex","animal_life_stage")], 
            by = c("individual_id" = "individual_id")) %>% 
  # 1 hourly
  left_join(.,  animlocs.1hourly %>%
              mutate(id = individual_id) %>% 
              tidyr::nest(data = -individual_id), 
            by = c("individual_id" = "individual_id")) %>% 
  dplyr::rename(animlocs.1hourly = data) %>% 
  # daily 
  left_join(.,  animlocs.daily %>%
            mutate(id = individual_id) %>% 
            tidyr::nest(data = -individual_id),
          by = c("individual_id" = "individual_id")) %>%
  dplyr::rename(animlocs.daily = data) %>%
  # weekly 
  left_join(.,  animlocs.weekly %>%
            mutate(id = individual_id) %>% 
            tidyr::nest(data = -individual_id),
          by = c("individual_id" = "individual_id")) %>%
  dplyr::rename(animlocs.weekly = data)

## ----Movement metrics-------------------------------------------------------------
## ----1h displacement-------------------------------------------------------------
animlocs.1hourly_sl <- 
  animlocs.1hourly |> filter(n() > 167) |> 
  mutate(d1h = step_lengths(animlocs.1hourly)*100000) |> 
  dplyr::select(individual_id, t_, d1h, x_, y_) |> 
  mutate(ymd = as.character(format(as.Date(t_), "%Y-%m-%d"))) |> 
  filter(!is.na(d1h))

## ----Maximum annual displacement distance-------------------------------------------------------------
#' Based on daily (weekly) relocations we calculated the maximum annual displacement 
#' distance from all pairwise comparisons. We included only individuals with at least 36 weeks (9 months) of data

locs7d <- flatten(data_resampled[,"animlocs.weekly"]) %>% bind_rows() %>% tibble() %>%
  mutate(year = as.numeric(strftime(t_,format="%Y")),
         id.year = paste(id,year,sep="_")) |>   
  group_by(id.year)  |> filter(n() >= 36) |> ungroup()

mean.coord <- locs7d |> group_by(id.year) |> 
  mutate(mean.x = mean(x_, na.rm=T),
         mean.y = mean(y_, na.rm=T)) |> 
  dplyr::select(id.year, mean.x, mean.y) |> distinct()

locs7d.sf <- sf::st_as_sf(locs7d,
                           coords = c("x_", "y_"),
                           crs = 4326)

moveObjSplitTime <- split(locs7d.sf, locs7d.sf$id.year)
maxNetDispL <- lapply(moveObjSplitTime, function(x){max(sf::st_distance(x))})
maxNetDisp <- do.call("rbind",maxNetDispL)

rm(moveObjSplitTime);rm(maxNetDispL);rm(locs7d);rm(locs7d.sf)

dmax12m <- 
  if(is.null(maxNetDisp)) NULL else {
  data.frame(keyName=row.names(maxNetDisp), dmax12m=maxNetDisp[,1], row.names=NULL) |> 
  filter(!is.na(dmax12m)) |> 
  mutate(year = str_split(keyName, "_", simplify = TRUE)[,2],
         individual_id = str_split(keyName, "_", simplify = TRUE)[,1]) |> 
  mutate(id.year = paste(individual_id,year,sep="_")) |>  left_join(mean.coord, by = "id.year") |> 
  dplyr::select(individual_id,year,dmax12m,mean.x, mean.y)
  }

rm(mean.coord)

## ----Summarize movement metrics per individual-------------------------------------------------------------

# ----Function d1h-------------------------------------------------------------
FDispl1h<-function(x)
  {
  # Check if the input is NULL
  if (is.null(x)) {
    # Create a placeholder dataframe with NA values
    dats <- data.frame(individual_id = NA,n1h = NA,D1h.mean = NA,D1h.median = NA,D1h.cv = NA,D1h.95 = NA,D1h.05 = NA)
  } else {
  individual_id <- with(x, tapply(as.character(x$individual_id),individual_id, unique))
  
  # Get sample size per individual
  n1h<-as.numeric(with(x, tapply(as.character(x$t_),individual_id, length)))
  
   # 1hr Displacement
  D1h.mean <- as.numeric(with(x, tapply(x$d1h+0.001,individual_id, mean, na.rm=T)))
  D1h.median <- as.numeric(with(x, tapply(x$d1h+0.001,individual_id, median, na.rm=T)))
  D1h.cv <- as.numeric(with(x, tapply(x$d1h+0.001,individual_id, function(x) sd(x, na.rm=T) / mean(x, na.rm=T))))
  D1h.95 <- as.numeric(with(x, tapply(x$d1h+0.001,individual_id, quantile,.95, na.rm=T)))
  D1h.05 <- as.numeric(with(x, tapply(x$d1h+0.001,individual_id, quantile,.05, na.rm=T)))
  
  # build dataframe
  dats<-data.frame(individual_id,
                   n1h,
                   D1h.mean,D1h.median,D1h.cv,D1h.95,D1h.05)
  
  return(dats)
  }
  }

Displ1h <- FDispl1h(animlocs.1hourly_sl[,c("individual_id","t_","d1h")])

## ----function to summarize Max12m Displacements------
FMaxDispl12m<-function(x)
  {
    # Check if the input is NULL
    if (is.null(x)) {
      # Create a placeholder dataframe with NA values
      dats <- data.frame(individual_id = NA,n.max12m.years = NA,
                         dmax12m.mean = NA,dmax12m.median = NA,dmax12m.cv = NA,dmax12m.95 = NA,dmax12m.05 = NA)
    } else {
  individual_id <- with(x, tapply(as.character(x$individual_id),individual_id, unique))
  
  # Get sample size per indivindividual_idual
  n.max12m.years<-as.numeric(with(x, tapply(x$year,individual_id, length)))
  
  # 24hr Displacement
  dmax12m.mean<-as.numeric(with(x, tapply(x$dmax12m+0.001,individual_id, mean, na.rm=T)))
  dmax12m.median<-as.numeric(with(x, tapply(x$dmax12m+0.001,individual_id, median, na.rm=T)))
  dmax12m.cv<-as.numeric(with(x, tapply(x$dmax12m+0.001,individual_id, function(x) sd(x, na.rm=T) / mean(x, na.rm=T))))
  dmax12m.95<-as.numeric(with(x, tapply(x$dmax12m+0.001,individual_id, quantile,.95, na.rm=T)))
  dmax12m.05<-as.numeric(with(x, tapply(x$dmax12m+0.001,individual_id, quantile,.05, na.rm=T)))
  
  # build dataframe
  dats<-data.frame(individual_id,n.max12m.years,
                   dmax12m.mean,dmax12m.median,dmax12m.cv,dmax12m.95,dmax12m.05)
  
  return(dats)
    }
    }

MaxDispl12m <- FMaxDispl12m(dmax12m)

## ----Build database with summary values--------------------
#' ## PROBLEM - this does not work when some metrics could not be computed
MoveTrait.v0 <- full_join(Displ1h, MaxDispl12m, by = join_by(individual_id)) %>% filter(if_any(everything(), ~ !is.na(.)))


movedata2 <- animlocs.1hourly %>% 
  mutate(study_id = as.integer64(study_id),
         individual_id = individual_id,
         species = taxon_canonical_name,
         bodymass_g = animal_mass,
         sex = sex,
         lifestage = animal_life_stage) |> 
  group_by(individual_id) %>% 
  mutate(mean.longitude = mean(x_,na.rm=T),
         mean.latitude = mean(y_,na.rm=T)) %>% 
  dplyr::select(study_id,individual_id,species,bodymass_g,sex,lifestage,mean.longitude,mean.latitude) %>% 
  mutate(individual_id = as.character(individual_id)) %>% 
  distinct()

DBMoveTrait.v02 <- 
movedata2 %>% 
  left_join(MoveTrait.v0, by = "individual_id") %>% 
  droplevels()

#' There are still list elements in the metadata - only keep first element!
DBMoveTrait.v02 <-
DBMoveTrait.v02 %>%
    mutate(lifestage = map_chr(lifestage, first),
           bodymass_g = map_chr(bodymass_g, first))

DBMoveTrait.v02$bodymass_g <- as.numeric(DBMoveTrait.v02$bodymass_g)

## ----Save database with summaries--------------------
#pathfolder_summary <- "/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/MoveTraitsDatabase_Git/MoveTraits_Git/DATA/output_movebank/trait_summaries/"
#saveRDS(DBMoveTrait.v02,
#        paste0(pathfolder_summary,DBMoveTrait.v02$individual_id,".rds"))  


## ----Build full database including raw metrics data--------------------

b <- 
  DBMoveTrait.v02 %>%
  tidyr::nest(data = -individual_id) %>% 
  dplyr::select(-data) %>% # just a quick trick to keep things flowing.
  left_join(., DBMoveTrait.v02[!duplicated(DBMoveTrait.v02$individual_id),], 
            by = c("individual_id" = "individual_id")) %>% 
  
  # 1 hourly
  {if (!is.null(dmax12m)) left_join(.,  animlocs.1hourly_sl %>%
              data.frame %>% 
              mutate(individual_id = as.character(individual_id)) |> 
              dplyr::select("individual_id","t_","d1h","x_","y_") %>% 
              tidyr::nest(Displ.1h = -individual_id), 
            by = c("individual_id" = "individual_id")) else .} %>% 
  
  # Dmax12m
  {if (!is.null(dmax12m)) left_join(.,  dmax12m %>%
              dplyr::select("individual_id","year","dmax12m","mean.x", "mean.y") %>% 
              tidyr::nest(MaxDispl.12m = -individual_id), 
            by = c("individual_id" = "individual_id")) else .} 



# Perform the join with conditional handling for NULL df_B
# if (is.null(dmax12m)) {
#   MoveTrait.v0_spatial %>% mutate(data = list(NULL))
# } else {
#   dmax12m.n <- dmax12m %>%
#     dplyr::select("individual_id","year","dmax12m","mean.x", "mean.y") %>% 
#     tidyr::nest(data = -individual_id)
#   
#   left_join(df_A, df_B_nested, by = "individual")
# }


  
## ----Save full database including raw metrics data--------------------
#pathfolder_spatial <- "/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/MoveTraitsDatabase_Git/MoveTraits_Git/DATA/movebank/trait_summaries_spatial/"
#saveRDS(MoveTrait.v0_spatial,paste0(pathfolder_spatial,MoveTrait.v0_spatial$individual_id,".rds"))  


length(a)
length(b)
dplyr::bind_rows(a, b)



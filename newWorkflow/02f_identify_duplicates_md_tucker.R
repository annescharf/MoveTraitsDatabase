library(mapview)


MoveTrait.v0.1 <- readRDS(file=paste0(pthdb,"MoveTrait.v0.1_individual.sum_20250304.rds"))

## ----Hebblewhite cervus elaphus/canadensis-------------------------------------------------------------

markheb.cervus <- MoveTrait.v0.1[MoveTrait.v0.1$contact_person_name %in% 
                 c("Mark Hebblewhite", "mark.hebblewhite (Mark Hebblewhite)") &
               MoveTrait.v0.1$species %in% c("Cervus elaphus", "Cervus canadensis"),]

table(markheb.cervus$source)

markheb.cervus[,c("tracking_start_date","tracking_end_date")]
min(markheb.cervus$tracking_start_date,na.rm=T)
max(markheb.cervus$tracking_end_date,na.rm=T)

# plot data - same area?
mapmarkheb.cervus <- markheb.cervus  |>  
  filter(!is.na(mean.longitude)) |>  
  filter(!is.na(mean.latitude)) 
mapmarkheb.cervus <- sf::st_as_sf(mapmarkheb.cervus, coords = c("mean.longitude", "mean.latitude"))
sf::st_crs(mapmarkheb.cervus) <- 4326
mapviewOptions(fgb = FALSE)
mapview(mapmarkheb.cervus,zcol = "source")

## same area, overlapping time frame, more individuals in movebank - keep movebank!

## ----Hebblewhite canis lupus-------------------------------------------------------------

markheb.canis <- MoveTrait.v0.1[MoveTrait.v0.1$contact_person_name %in% 
                                   c("Mark Hebblewhite", "mark.hebblewhite (Mark Hebblewhite)") &
                                   MoveTrait.v0.1$species %in% c("Canis lupus"),]

table(markheb.canis$source)

markheb.canis[,c("tracking_start_date","tracking_end_date")]
max(markheb.canis$tracking_end_date,na.rm=T)

# plot data - same area?
mapmarkheb.canis <- markheb.canis  |>  
  filter(!is.na(mean.longitude)) |>  
  filter(!is.na(mean.latitude)) 
mapmarkheb.canis <- sf::st_as_sf(mapmarkheb.canis, coords = c("mean.longitude", "mean.latitude"))
sf::st_crs(mapmarkheb.canis) <- 4326
mapviewOptions(fgb = FALSE)
mapview(mapmarkheb.canis,zcol = "source")

## NO overlapping time frame - keep both!


## ----Kazensky Equus hemionus-------------------------------------------------------------

equus <- MoveTrait.v0.1[MoveTrait.v0.1$species %in% c("Equus hemionus"),]

table(equus$contact_person_name)
table(equus$source)

equus[,c("tracking_start_date","tracking_end_date")]
min(equus$tracking_start_date,na.rm=T)
max(equus$tracking_end_date,na.rm=T)

## but NO overlapping time frame - keep both!

## ----Young Puma concolor -------------------------------------------------------------

puma <- MoveTrait.v0.1[MoveTrait.v0.1$species %in% c("Puma concolor"),]

table(puma$contact_person_name,puma$source)

puma[,c("tracking_start_date","tracking_end_date")]
min(puma$tracking_start_date,na.rm=T)
max(puma$tracking_end_date,na.rm=T)

## NO overlapping time frame - keep both!


## ----Hopkraft Connochaetes taurinus -------------------------------------------------------------

gnu <- MoveTrait.v0.1[MoveTrait.v0.1$species %in% c("Connochaetes taurinus"),]

table(gnu$contact_person_name,gnu$source)

gnu <- gnu[gnu$contact_person_name != c("jstabach (Jared)"),]

gnu[,c("tracking_start_date","tracking_end_date")]

min(gnu$tracking_start_date,na.rm=T)
max(gnu$tracking_end_date,na.rm=T)

# plot data - same area?
mapgnu <- gnu  |>  
  filter(!is.na(mean.longitude)) |>  
  filter(!is.na(mean.latitude)) 
mapgnu <- sf::st_as_sf(mapgnu, coords = c("mean.longitude", "mean.latitude"))
sf::st_crs(mapgnu) <- 4326
mapviewOptions(fgb = FALSE)
mapview(mapgnu,zcol = "source")

## NO overlapping time frame - keep movebank, remove Tucker  

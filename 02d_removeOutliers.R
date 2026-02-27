# ---
# title: "MoveTraits Database"
# author: "Anne K. Scharf"
# date: "February 2025"
# ---

### SCRIPT IN THE DOINGS; NOTHING WORKING YET!!!!

## UPDATE SEPTEMBER 2025 - WORKING ON AN IMPLEMENTATION TO FILTER UNSUAL SPEEDS BY SPECIES ##

### in this script: 
## outliers according to speed are removed 
## outliers according to distance are removed

### ToDo: clean up script, adjust folder names, think about how best do it...
#### IDEA:
## 1st round, calculate speed and distance per species and make table with percentilX values as cutoff
## next rounds use this cutoff for filtering per species
## if new species is added, new cutoff value gets added to table
## add column with nb of individuals cutoff was calculated from, and date of value creation

## Todo at some point:
## - how to deal with dead animals
## - how to deal with undefined deployments: tag in car, house, plane before or after on animal -> vehicles will get removed with speed, but sitting in a house will be tricky
## - difference between sitting in a house, dead, nesting/dening...

library(amt)
library(lubridate)
library(units)
library(sf)
library(dplyr)
library(EnvStats)
library(scales)
library(ggplot2)
library(scales)

pathTOfolder <- "./DATA/MoveTraitsData/"
referenceTableStudies <- readRDS(paste0(pathTOfolder,"/referenceTableStudies_ALL_original.rds"))

pthamt1h <- paste0(pathTOfolder,"4.MB_indv_amt_1h/")
dir.create(paste0(pathTOfolder,"5.speed_per_species"))
pthspsSpeed <- paste0(pathTOfolder,"5.speed_per_species/")
#dir.create(paste0(pathTOfolder,"6.steplenght_per_species"))
#pthspsStplngh <- paste0(pathTOfolder,"6.steplenght_per_species/")


dir.create(paste0(pathTOfolder,"5.MB_indv_amt_1h_outlspeed"))
pthamt1hOutl <- paste0(pathTOfolder,"5.MB_indv_amt_1h_outlspeed/")
#dir.create(paste0(pathTOfolder,"6.MB_indv_amt_1h_outlspeed_dist"))
#pthamt1hOutlDist <- paste0(pathTOfolder,"6.MB_indv_amt_1h_outlspeed_dist/")

### create table with speeds quantiles, nb of indiv, date when created. Use this table to filter out speeds
tb_per_sps_L <- split(referenceTableStudiesUsed, referenceTableStudiesUsed$species)

#sps_tb <- tb_per_sps_L[[267]]

### annotate species with class
#library(taxize)
#out <- classification(as.character(unique(referenceTableStudies$species)), db = 'itis')
# 
# # Extract the 'class' rank for each species
# class_vec <- sapply(out, function(x) {
#   if (is.null(x) || !is.data.frame(x) || !"class" %in% x$rank) return(NA)
#   x$name[x$rank == "class"]
# })
# 
# class_df <- tibble::enframe(class_vec)
# 
# class_df[is.na(class_df$value),2] <- "Aves"
# class_df[class_df$name %in% c("Sapajus macrocephalus","Tapirus bairdii","Bison bison",
#                               "Martes pennanti","Myotis daubentoni","Capra hircus,Ovis aries"),2] <- "Mammalia"
# class_df[class_df$name %in% c("Chelonoidis donfaustoi","Chelonoidis hoodensis",
#                               "Chelonoidis porteri","Chelonoidis ,Chelonoidis hoodensis,Chelonoidis porteri,Chelonoidis donfaustoi"),2] <- "Reptilia"
# class_df$cutoff <- ifelse(class_df$value %in% "Aves", 180,
#                           ifelse(class_df$value %in% "Mammalia", 25, 180))
# 
# class_df <- merge(referenceTableStudies[,c("fileName","species")],
#                    class_df,
#                    by.x = "species",by.y="name")
# 
# 
# saveRDS(class_df,"referenceTableStudies_taxonomy.rds")
class_df <- readRDS("referenceTableStudies_taxonomy.rds")


# Calculate speeds
# lapply(tb_per_sps_L, function(sps_tb){
#   flsAMT <- as.character(sps_tb$fileName)
#   # ind <- flsAMT[1]
#   speedL <- lapply(flsAMT, function(ind)try({
#   amt_tr_1h <- readRDS(paste0(pthamt1h,ind))
#   ind_speed <- speed(amt_tr_1h,append_na=F) * 3.6 *100000 # km/h
#   return(ind_speed)
#   }))
#   sps <- gsub(" ","_",as.character(unique(sps_tb$species)))
#   saveRDS(speedL, file=paste0(pthspsSpeed,sps,".rds"))
# })

## remove speeds higher than threshold 25 km/h for mammals and higher than 180 km/h for birds
library(Hmisc)
library(dplyr)
library(move2)
flsMVs <- list.files(pthamt1h, full.names = F)
flsMVs[flsMVs %in% class_df$fileName]

results <- lapply(flsMVs, function(indPth)try({
  amt_tr_1h <- readRDS(paste0(pthamt1h,indPth))
  maxspeed <- class_df[class_df$fileName %in% indPth,4] # determine maxspeed, depending on taxonomy 

  ind_speed <- speed(amt_tr_1h) * 3.6 *100000 # speed in km/h 
  amt_tr_1h$speed_km_h <- ind_speed # append speed to trkxy
  amt_tr_1h_filtered <- amt_tr_1h %>% filter(speed_km_h <= maxspeed | is.na(amt_tr_1h$speed_km_h)) # filter by maxspeed, keep trailing position
  amt_tr_1h_filtered <- dplyr::select(amt_tr_1h_filtered,-speed_km_h)
  
  saveRDS(amt_tr_1h_filtered, file=paste0(pthamt1hOutl,indPth))
}))


# END EDITS ANNE OCT 2025




  ## remove speeds higher than threshold 20 -- remove top 0.15%
  library(dplyr)
  library(move2)
  flsMVs <- list.files(pthamt1h, full.names = F)
  # indPth <- flsMVs[1000]
  start_time <- Sys.time()
  maxspeed <- 20
  results <- lapply(flsMVs, function(indPth)try({
    print(indPth)
    vultr <- readRDS(paste0(pthamt1h,indPth))
    while(any(mt_speed(vultr, units="m/s")>set_units(maxspeed, m/s), na.rm = TRUE)){
      vultr <- vultr %>% filter(mt_speed(., units="m/s")<=set_units(maxspeed, m/s) | is.na(mt_speed(., units="m/s")))
    }
    saveRDS(vultr, file=paste0(pthamt1hOutl,indPth))
  }))
  end_time <- Sys.time()
  end_time - start_time # 40min
  
  is.error <- function(x) inherits(x, "try-error")
  table(vapply(results, is.error, logical(1)))
  names(results) <- seq_along(results)
  results[vapply(results, is.error, logical(1))]
  
  ### remove outliers based on distance
  ## check distribution of speeds
  flsMVs <- list.files(pthamt1hOutl, full.names = T)
  indPth <- flsMVs[1]
  start_time <- Sys.time()
  distL <- lapply(flsMVs, function(indPth){
    vultr <- readRDS(indPth)
    vultr_dist <- mt_distance(vultr, units="m")
    return(vultr_dist)
  })
  end_time <- Sys.time()
  end_time - start_time #10min
  
  distAll <- unlist(distL)
  distAll <- distAll[!is.na(distAll)]
  hist(distAll)
  round(quantile(distAll, seq(0.9,1,0.01)),2)
  hist(distAll[distAll<50000])
  round(quantile(distAll, seq(0.999,1,0.00001)))
  
  dl <- distAll[distAll>1000000]
  
  
  ## remove distances higher than threshold 1000K km -- remove top 0.0001%
  library(dplyr)
  library(move2)
  flsMVs <- list.files(pthamt1hOutl, full.names = F)
  # indPth <- flsMVs[1000]
  start_time <- Sys.time()
  maxdist <- 1000000
  results <- lapply(flsMVs, function(indPth)try({
    print(indPth)
    vultr <- readRDS(paste0(pthamt1hOutl,indPth))
    vultr <- vultr %>% filter(mt_distance(., units="m")<=set_units(maxdist, m) | is.na(mt_distance(., units="m")))
    saveRDS(vultr, file=paste0(pthamt1hOutlDist,indPth))
  }))
  end_time <- Sys.time()
  end_time - start_time # 25min
  
  is.error <- function(x) inherits(x, "try-error")
  table(vapply(results, is.error, logical(1)))
  names(results) <- seq_along(results)
  results[vapply(results, is.error, logical(1))]
})
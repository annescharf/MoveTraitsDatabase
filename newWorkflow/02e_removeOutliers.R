# ---
# title: "MoveTraits Database"
# author: "Anne K. Scharf"
# date: "February 2025"
# ---

### SCRIPT IN THE DOINGS; NOTHING WORKING YET!!!!

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
# library(move2)
library(units)
library(sf)
library(dplyr)

pathTOfolder <- "./MoveTraitsData/"
referenceTableStudies <- readRDS(paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))
referenceTableStudiesUsed <- referenceTableStudies[referenceTableStudies$excluded=="no",]
# head(referenceTableStudiesUsed)
# summary(referenceTableStudiesUsed)

pthamt1h <- paste0(pathTOfolder,"4.MB_indv_amt_1h/")
dir.create(paste0(pathTOfolder,"5.speed_per_species"))
pthspsSpeed <- paste0(pathTOfolder,"5.speed_per_species/")
dir.create(paste0(pathTOfolder,"6.steplenght_per_species"))
pthspsStplngh <- paste0(pathTOfolder,"6.steplenght_per_species/")


dir.create(paste0(pathTOfolder,"5.MB_indv_amt_1h_outlspeed"))
pthamt1hOutl <- paste0(pathTOfolder,"5.MB_indv_amt_1h_outlspeed/")
dir.create(paste0(pathTOfolder,"6.MB_indv_amt_1h_outlspeed_dist"))
pthamt1hOutlDist <- paste0(pathTOfolder,"6.MB_indv_amt_1h_outlspeed_dist/")


### create table with speeds quantiles, nb of indiv, date when created. Use this table to filter out speeds
tb_per_sps_L <- split(referenceTableStudiesUsed, referenceTableStudiesUsed$species)

# sps_tb <- tb_per_sps_L[[1]]
lapply(tb_per_sps_L, function(sps_tb){
  flsAMT <- as.character(sps_tb$fileName)
  # ind <- flsAMT[1]
  speedL <- lapply(flsAMT, function(ind)try({
  amt_tr_1h <- readRDS(paste0(pthamt1h,ind))
  ind_speed <- speed(amt_tr_1h,append_na=F)
  return(ind_speed)
  }))
  sps <- gsub(" ","_",as.character(unique(sps_tb$species)))
  saveRDS(speedL, file=paste0(pthspsSpeed,sps,".rds"))
})
  

flsSP <- list.files(pthspsSpeed, full.names = F)
sps <- flsSP[2]
percentSpeedsL <- lapply(flsSP, function(sps){
  speedL <- readRDS(paste0(pthspsSpeed,sps))
  speedAll <- unlist(speedL)
  df <- data.frame(t(quantile(speedAll, c(0.8,0.90,0.95,0.99,1))))
  df$species <- gsub("_"," ",gsub(".rds","",sps))
  return(df)
})
percentil_speeds_sps <- do.call("rbind",percentSpeedsL)
saveRDS(percentil_speeds_sps, file=paste0(pathTOfolder,"percentil_speeds_sps"))

# HERE!!

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
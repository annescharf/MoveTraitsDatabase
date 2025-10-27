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
referenceTableStudies <- readRDS(paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))
referenceTableStudiesUsed <- referenceTableStudies[referenceTableStudies$excluded=="no",]
# head(referenceTableStudiesUsed)
# summary(referenceTableStudiesUsed)

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

lapply(tb_per_sps_L, function(sps_tb){
  flsAMT <- as.character(sps_tb$fileName)
  # ind <- flsAMT[1]
  speedL <- lapply(flsAMT, function(ind)try({
  amt_tr_1h <- readRDS(paste0(pthamt1h,ind))
  ind_speed <- speed(amt_tr_1h,append_na=F) * 3.6 *100000 # km/h
  return(ind_speed)
  }))
  sps <- gsub(" ","_",as.character(unique(sps_tb$species)))
  saveRDS(speedL, file=paste0(pthspsSpeed,sps,".rds"))
})

flsSP <- list.files(pthspsSpeed, full.names = F)
percentSpeedsL <- lapply(flsSP, function(sps){
  speedL <- readRDS(paste0(pthspsSpeed,sps))
  speedAll <- unlist(speedL)
  tryCatch(
    {
      if(is.numeric(speedAll)) {
  df <- data.frame(t(round(quantile(speedAll, c(0.8,0.90,0.95,0.99,1)),2)),
                   IQR = round(iqr(speedAll),2),
                   Tucker.cut = round(quantile(speedAll, 0.75) + 100 * iqr(speedAll),2),
                   n = length(speedAll),
                   n.rem = sum(speedAll > (quantile(speedAll, 0.75) + 100 * iqr(speedAll))))
  
  df$species <- gsub("_"," ",gsub(".rds","",sps))
  return(df)
      } else { NULL }
    }, error = function(e) NULL # Skips errors and returns NULL
  )
})

percentil_speeds_sps <- do.call("rbind",percentSpeedsL)
rownames(percentil_speeds_sps) <- NULL

saveRDS(percentil_speeds_sps, file=paste0(pathTOfolder,"percentil_speeds_sps.rds"))
write.csv(percentil_speeds_sps, file=paste0(pathTOfolder,"percentil_speeds_sps.csv"))

#percentil_speeds_sps <- readRDS("/Users/ahertel/Documents/Work/Study_MoveTraits/database v 0.0/MoveTraitsDatabase_Git/MoveTraits_Git/DATA/MoveTraitsData/percentil_speeds_sps.rds")


# Plot histograms of > 75% of data
pdf(paste0(pathTOfolder,"speed_histograms.pdf"))
lapply(flsSP, function(sps){
  speedL <- readRDS(paste0(pthspsSpeed,sps))
  speedAll <- unlist(speedL)
  tryCatch(
    {
      if(is.numeric(speedAll)) {
        speedAll <- speedAll[speedAll > quantile(speedAll, 0.75)]
        
        ggplot()+
          geom_histogram(aes(x=speedAll)) +
          scale_y_continuous(breaks = breaks_pretty(),limits = c(0,20),oob=squish) +
          labs(title  = gsub("_"," ",gsub(".rds","",sps)), x = "speed in km/hr") +
          geom_vline(xintercept = percentil_speeds_sps[percentil_speeds_sps$species %in% gsub("_"," ",gsub(".rds","",sps)),7], col = "red") 
        
      }  
    }, error = function(e) NULL # Skips errors and returns NULL
  )
}
)
dev.off()

# END EDITS ANNE OCT 2025
# HERE SHOULD COME A MANUAL PART OF DEFINING CUTOFF SPEEDS



# Speed histogramm mammals / birds

library(clipr)
classes <- read_clip_tbl()
classes <- classes[!is.na(classes$species),]

#MAMMALS
mam <- classes[classes$class %in% "mammal",1]
mam <- sub(" ", "_", mam)

all_files <- list.files(pthspsSpeed, full.names = F)
# Extract base names without .rds for filtering
file_species <- tools::file_path_sans_ext(basename(all_files))
# Filter files using the species vector
desired_files <- all_files[file_species %in% mam]

speed.mammal <- map_dfr(desired_files, function(f) {
  dat <- readRDS(paste0(pthspsSpeed,f))
  dat <- as.data.frame(unlist(dat))
  if (nrow(dat) > 0) {
    dat$species <- tools::file_path_sans_ext(basename(f))
    return(dat)
  } else {
    return(NULL)
  }
})

colnames(speed.mammal)[1] <- "speed"

#BIRDS
bird <- classes[classes$class %in% "bird",1]
bird <- sub(" ", "_", bird)

all_files <- list.files(pthspsSpeed, full.names = F)
# Extract base names without .rds for filtering
file_species <- tools::file_path_sans_ext(basename(all_files))
# Filter files using the species vector
desired_files <- all_files[file_species %in% bird]

speed.birds <- map_dfr(desired_files, function(f) {
  dat <- readRDS(paste0(pthspsSpeed,f))
  dat <- as.data.frame(unlist(dat))
  if (nrow(dat) > 0) {
    dat$species <- tools::file_path_sans_ext(basename(f))
    return(dat)
  } else {
    return(NULL)
  }
})

colnames(speed.birds)[1] <- "speed"

speed_cutoffs.m <- data.frame(t(round(quantile(speed.mammal$speed, c(0.8,0.90,0.95,0.99,1)),2)),
                 IQR = round(iqr(speed.mammal$speed),2),
                 Tucker.cut = round(quantile(speed.mammal$speed, 0.75) + 100 * iqr(speed.mammal$speed),2),
                 n = length(speed.mammal$speed),
                 n.rem = sum(speed.mammal$speed > (quantile(speed.mammal$speed, 0.75) + 100 * iqr(speed.mammal$speed))),
                 class = "mammal")
speed_cutoffs.b <- data.frame(t(round(quantile(speed.birds$speed, c(0.8,0.90,0.95,0.99,1)),2)),
                            IQR = round(iqr(speed.birds$speed),2),
                            Tucker.cut = round(quantile(speed.birds$speed, 0.75) + 100 * iqr(speed.birds$speed),2),
                            n = length(speed.birds$speed),
                            n.rem = sum(speed.birds$speed > (quantile(speed.birds$speed, 0.75) + 100 * iqr(speed.birds$speed))),
                            class = "bird")

speed_cutoffs <- rbind(speed_cutoffs.m,speed_cutoffs.b)


# Plot histograms of > 75% of data
pdf(paste0(pathTOfolder,"speed_histogram_mammal_B.pdf"))
        ggplot()+
          geom_histogram(aes(x=speed.mammal$speed)) +
          scale_y_continuous(breaks = breaks_pretty(),limits = c(0,50),oob=squish) +
          scale_x_continuous(breaks = breaks_pretty(),limits = c(0,100))+ #,limits = c(0,100)
          labs(title  = "Mammals", x = "speed in km/hr") +
          geom_vline(xintercept = 25, col = "red") 
dev.off()

#

tr <- 
  speed.birds |> 
  filter(speed > 120) |> 
  filter(speed < 160)

table(tr$species)





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
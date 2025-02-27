# ---
# title: "MoveTraits Database"
# author: "Anne K. Scharf"
# date: "February 2025"
# ---

### in this script: 
## data are transformed into a amt track_xyt class
## data are subset to 1h+-15min interval 

library(amt)
library(lubridate)
library(move2)
library(units)
library(sf)
library(dplyr)

pathTOfolder <- "./DATA/MoveTraitsData/"
referenceTableStudies <- readRDS(paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))
referenceTableStudiesUsed <- referenceTableStudies[referenceTableStudies$excluded=="no",]
head(referenceTableStudiesUsed)
summary(referenceTableStudiesUsed)

pthClean <- paste0(pathTOfolder,"2.MB_indv_mv2_clean/")
dir.create(paste0(pathTOfolder,"4.MB_indv_amt_1h"))
pthamt1h <- paste0(pathTOfolder,"4.MB_indv_amt_1h/")

flsMV <- list.files(pthClean, full.names = F)
done <- list.files(pthamt1h, full.names = F)
flsMV2 <- flsMV[!flsMV%in%done]

#flsMV2 <- as.character(referenceTableStudiesUsed$fileName)

####### convert tracks to amt and thin to 1h ###########

# ind <- flsMV2[20]
# ind <- "1764627_1764834.rds"
start_time <- Sys.time()
results <- lapply(flsMV2, function(ind)try({
  print(ind)
  mv2 <- readRDS(paste0(pthClean,ind))
  ## only keep attr of interest. all can be found in 'referenceTableStudies_ALL_excludedColumn.rds'
  mv2 <- mt_as_event_attribute(mv2, c("individual_local_identifier","individual_id","tag_local_identifier","study_id"))
  ## make all tracks per individual, gaps between deployment will be accounted for as any other gaps
  mt_track_id(mv2) <- "individual_local_identifier"
  mv2 <- dplyr::arrange(mv2,mt_time(mv2))
  ## convert to amt track_xyt class
  amt_tr <- track(mv2[,c("individual_local_identifier","tag_local_identifier","individual_id","study_id")], 
                  x=sf::st_coordinates(mv2)[,1],
                  y=sf::st_coordinates(mv2)[,2],
                  t=mt_time(mv2),
                  crs = sf::st_crs(mv2))
  ## remove geometry column and deployment_id if present as they are of no use
  amt_tr <- amt_tr %>% dplyr::select(!geometry)
  if("deployment_id"%in%names(amt_tr)){amt_tr <- amt_tr %>% select(!deployment_id)}
  
  amt_tr_1h <- amt_tr |> track_resample(rate = hours(1),
                                        tolerance = minutes(15))
  
  saveRDS(amt_tr_1h, file=paste0(pthamt1h,ind))
}))
end_time <- Sys.time()
end_time - start_time # 

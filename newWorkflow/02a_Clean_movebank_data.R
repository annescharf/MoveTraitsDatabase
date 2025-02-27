# ---
# title: "MoveTraits Database"
# author: "Anne K. Scharf"
# date: "February 2025"
# ---

### in this script:
## data are cleaned: empty locations, "0,0" coordinates and duplicated timestamps are removed

library(move2)
library(units)
library(dplyr)

## in case in parallel is an option
# library(doParallel)
# library(plyr)
# mycores <- detectCores()-1
# registerDoParallel(mycores)
# library(dplyr)

pathTOfolder <- "./DATA/MoveTraitsData/"
pthDownld <- paste0(pathTOfolder,"1.MB_indv_mv2/")
dir.create(paste0(pathTOfolder,"2.MB_indv_mv2_clean"))
pthClean <- paste0(pathTOfolder,"2.MB_indv_mv2_clean/")

flsMV <- list.files(pthDownld, full.names = F)
done <- list.files(pthClean, full.names = F)
flsMV <- flsMV[!flsMV%in%done]

# indPth <- flsMV[10]

## remove empty locs, 0,0 corrds and duplicated ts
start_time <- Sys.time()
lapply(flsMV, function(indPth){
  # llply(flsMV, function(indPth){
  mv2 <- readRDS(paste0(pthDownld,indPth))
  if(!mt_is_track_id_cleaved(mv2)){mv2 <- mv2 |> dplyr::arrange(mt_track_id(mv2))} ## order by tracks
  if(!mt_is_time_ordered(mv2)){mv2 <- mv2 |> dplyr::arrange(mt_track_id(mv2),mt_time(mv2))} # order time within tracks
  if(!mt_has_no_empty_points(mv2)){mv2 <- mv2[!sf::st_is_empty(mv2),]} ## remove empty locs
  
  ## sometimes only lat or long are NA
  crds <- sf::st_coordinates(mv2)
  rem <- unique(c(which(is.na(crds[,1])),which(is.na(crds[,2]))))
  if(length(rem)>0){mv2 <- mv2[-rem,]}
  
  ## remove 0,0 coordinates
  rem0 <- which(crds[,1]==0 & crds[,2]==0)
  if(length(rem0)>0){mv2 <- mv2[-rem0,]}
  
  ## retain the duplicate entry which contains the least number of columns with NA values
  mv2 <- mv2 %>%
    mutate(n_na = rowSums(is.na(pick(everything())))) %>%
    arrange(n_na) %>%
    mt_filter_unique(criterion='first') %>% # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA. 
    dplyr::arrange(mt_track_id()) %>%
    dplyr::arrange(mt_track_id(),mt_time())
  
  saveRDS(mv2, file=paste0(pthClean,indPth))
} )
# } ,.parallel = T)
end_time <- Sys.time()
end_time - start_time # 


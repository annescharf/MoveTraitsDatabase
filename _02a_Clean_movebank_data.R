
### in this script:
## data are cleaned: empty locations, "0,0" coordinates and duplicated timestamps are removed
## ToDo: adjust folder names

library(move2)
library(units)

## in case in parallel is an option
# library(doParallel)
# library(plyr)
# mycores <- detectCores()-1
# registerDoParallel(mycores)
# library(dplyr)

pathTOfolder <- "./MoveTraitsData/"
pthDownld <- paste0(pathTOfolder,"1.MB_indv_mv2/")
dir.create(paste0(pathTOfolder,"2.MB_indv_mv2_clean"))
pthClean <- paste0(pathTOfolder,"2.MB_indv_mv2_clean/")



flsMV <- list.files(pthDownld, full.names = F)
done <- list.files(pthClean, full.names = F)
flsMV <- flsMV[!flsMV%in%done]


# indPth <- flsMV[10]
# indPth <- "2038660251_G31258 - B536 - 7216 - Wild.rds"


## remove empty locs, 0,0 corrds and duplicated ts
start_time <- Sys.time()
lapply(flsMV, function(indPth){
  # llply(flsMV, function(indPth){
  mv2 <- readRDS(paste0(pthDownld,indPth))
  ## remove empty locs
  mv2 <- mv2[!sf::st_is_empty(mv2),]
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


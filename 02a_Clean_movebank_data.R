
### in this script:
## data are cleaned: empty locations, "0,0" coordinates and duplicated timestamps are removed
## ToDo: adjust folder names

library(move2)
library(units)


library(doParallel)
library(plyr)
mycores <- detectCores()-1
registerDoParallel(mycores)
library(dplyr)

genPath <- "/home/ascharf/Documents/Projects/Drylands/UDsizeChange/"
pthDownld <- "1.vultureIndv_mv2/"
filePath <- paste0(genPath,pthDownld)
dir.create(paste0(genPath,"2.vultureIndv_mv2_clean_empty_duply"))
pthClean <- "2.vultureIndv_mv2_clean_empty_duply/"
savePath <- paste0(genPath,pthClean)


flsMV <- list.files(filePath, full.names = F)
done <- list.files(savePath, full.names = F)
flsMV <- flsMV[!flsMV%in%done]


# indPth <- flsMV[10]
# indPth <- "2038660251_G31258 - B536 - 7216 - Wild.rds"


## remove empty locs
## remove duplicated ts
start_time <- Sys.time()
lapply(flsMV, function(indPth){
  # llply(flsMV, function(indPth){
  vultr <- readRDS(paste0(filePath,indPth))
  ## remove empty locs
  vultr <- vultr[!sf::st_is_empty(vultr),]
  ## sometimes only lat or long are NA
  crds <- sf::st_coordinates(vultr)
  rem <- unique(c(which(is.na(crds[,1])),which(is.na(crds[,2]))))
  if(length(rem)>0){vultr <- vultr[-rem,]}
  
  ## remove 0,0 coordinates
  rem0 <- which(crds[,1]==0 & crds[,2]==0)
  if(length(rem0)>0){vultr <- vultr[-rem0,]}
  
  ##retain the duplicate entry which contains the least number of columns with NA values
  vultr <- vultr %>%
    mutate(n_na = rowSums(is.na(pick(everything())))) %>%
    arrange(n_na) %>%
    mt_filter_unique(criterion='first') %>% # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA. 
    dplyr::arrange(mt_track_id()) %>%
    dplyr::arrange(mt_track_id(),mt_time())
  
  saveRDS(vultr, file=paste0(savePath,indPth))
} )
# } ,.parallel = T)
end_time <- Sys.time()
end_time - start_time # 4.5h


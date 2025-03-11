# ---
# title: "MoveTraits Database"
# author: "Anne Hertel"
# date: "March 2025"
# ---

### in this script: 
## open access studies where owners have not approved reuse are set to "exclude = yes" in reference table

library(lubridate);library(metafor);library(tidyverse);library(amt);library(Hmisc)
library(adehabitatHR); library(move2); library(epitools); library(suncalc); library(purrr); library(bit64)

## ----Import movement data per individual-------------------------------------------------------------
pathTOfolder <- "./DATA/MoveTraitsData/"

metadata <- readRDS(paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn.rds"))
metadata[metadata$MBid %in% 1541820092,"excluded"] <- "yes" # exclude Cagan study
metadata[metadata$fileName %in%  c("1120749252_3069649656.rds",
                                   "1120749252_3069649678.rds",
                                   "1120749252_3069649685.rds",
                                   "2950149_2950167.rds") ,"excluded"] <- "yes" # exclude coaties

metadata[metadata$fileName %in%  c("17469219_1515864240.rds","17469219_171242772.rds","17469219_171242945.rds", 
                                   "17469219_171243042.rds","17469219_173320297.rds","17469219_176963464.rds", 
                                   "17469219_18418658.rds","17469219_18427263.rds","17469219_18427289.rds",  
                                   "17469219_18427333.rds","17469219_2110127141.rds","17469219_497810078.rds", 
                                   "17469219_506756344.rds","17469219_564630857.rds","17469219_75153875.rds",  
                                   "17469219_876313552.rds") , "excluded"] <- "yes" # exclude great blue heron

saveRDS(metadata, paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn_excludedStudies.rds"))

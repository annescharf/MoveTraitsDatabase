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

saveRDS(metadata, paste0(pathTOfolder,"/referenceTableStudies_ALL_excludedColumn_excludedStudies.rds"))

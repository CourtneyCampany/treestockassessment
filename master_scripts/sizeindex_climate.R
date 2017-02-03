

# read size index and climate date ----------------------------------------
source("functions_and_packages/functions.R")

oz_si <- read.csv("calculated_data/oz_sizeindex.csv")
climate30 <- read.csv("climate_data/nursery_climate_history.csv")
temp5 <- read.csv("climate_data/max_temp_history.csv")

# extract and save species list -------------------------------------------
spec <- data.frame(unique(oz_si$species))
  names(spec) <- "genus_species_variety"
genusspec <- data.frame(unique(oz_si$genus_species))
  names(genusspec) <- "genus_species"

write.csv(spec, "calculated_data/species_list.csv", row.names = FALSE)
write.csv(genusspec, "calculated_data/genusspecies_list.csv", row.names = FALSE)


# means dataset of size index parameters ----------------------

  library(doBy)  
  oz_si_agg <- summaryBy(calliper300+rcd+height_m+sizeindex ~ species + genus_species + nursery+volume +
                           climate_region+origin+leaf_type, 
                         data=oz_si, FUN=c(mean, se))

  ##remove trees with only one tree (AR_OG - ETT / FBeng500 - DPH / FBenj100 - DPH / LIT200 - ETT) / WF100 - alp)
  ##as data is clean here just remove values with NA (and drop levels)
  oz_si_agg2 <- oz_si_agg[complete.cases(oz_si_agg),]
  

  
# merge size index with nursery climate history  -----------------------
  #sizeindex_clim <- merge(oz_si_agg2, temp5, by="nursery")
  
  si_means_clim <- merge(oz_si_agg2, climate30, by="nursery", all=TRUE)
  
  si_clim <-   merge(oz_si, climate30, by="nursery")

  
#write data setes    
write.csv(si_means_clim, "master_scripts/si_means_climate.csv", row.names = FALSE) 
write.csv(si_clim, "master_scripts/si_climate.csv", row.names = FALSE)  
  
  
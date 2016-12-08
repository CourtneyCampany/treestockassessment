

# read size index and climate date ----------------------------------------
source("functions_and_packages/functions.R")

oz_si <- read.csv("calculated_data/oz_sizeindex.csv")
climate30 <- read.csv("climate_data/nursery_climate_history.csv")
temp5 <- read.csv("climate_data/max_temp_history.csv")


####names of nurseries are not same format

# combine size index parameters with all climate data ----------------------

  ##create a means dataset of size index parameters, merge this with climate
  library(doBy)  
  oz_si_agg <- summaryBy(calliper300+rcd+height_m+sizeindex ~ species + nursery+volume, data=oz_si, FUN=c(mean, se))

  spec <- data.frame(unique(oz_si_agg$species))
  ##remove trees with only one tree (AR_OG - ETT / FBeng500 - DPH / FBenj100 - DPH / LIT200 - ETT) / WF100 - alp)
  ##as data is clean here just remove values with NA (and drop levels)
  oz_si_agg2 <- complete.cases(oz_si_agg)
  
  
tree_dat <-
  
  test_agg <- summaryBy(calliper300+rcd+height_m+sizeindex ~ species + nursery+volume, data=test, FUN=c(mean, se))

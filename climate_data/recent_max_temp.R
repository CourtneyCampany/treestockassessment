

# read and calculate temp yearly averages ---------------------------------


temp_dat <- read.csv("climate_data/5yr_temp.csv")

library(doBy)

mean7yr <- summaryBy(mean_max_temp ~ nursery, data= temp_dat, keep.names = TRUE)
  names(mean7yr)[2] <- "mean_max_temp_7yr"
  
mean6yr <- summaryBy(mean_max_temp ~ nursery, data= temp_dat[temp_dat$year != "2010",], keep.names = TRUE)
  names(mean6yr)[2] <- "mean_max_temp_6yr"
  
mean5yr <- summaryBy(mean_max_temp ~ nursery, data= temp_dat[temp_dat$year %in% 
                                                      c("2012", "2013", "2014", "2015", "2016"),], keep.names = TRUE)
  names(mean5yr)[2] <- "mean_max_temp_5yr"
  
mean4yr <- summaryBy(mean_max_temp ~ nursery, data= temp_dat[temp_dat$year %in% 
                                                      c("2013", "2014", "2015", "2016"),], keep.names = TRUE)
  names(mean4yr)[2] <- "mean_max_temp_4yr"
  
mean3yr <- summaryBy(mean_max_temp ~ nursery, data= temp_dat[temp_dat$year %in% c("2014", "2015", "2016"),],
                     keep.names = TRUE)
  names(mean3yr)[2] <- "mean_max_temp_3yr"
  
mean2yr <- summaryBy(mean_max_temp ~ nursery, data= temp_dat[temp_dat$year %in% c("2015", "2016"),], 
                     keep.names = TRUE)
  names(mean2yr)[2] <- "mean_max_temp_2yr"
  
mean1yr <- summaryBy(mean_max_temp ~ nursery, data= temp_dat[temp_dat$year == "2016",], 
                     keep.names = TRUE)
  names(mean1yr)[2] <- "mean_max_temp_1yr"
  

# create temp history dfr and write ---------------------------------------

temp_history<- Reduce(function(...) merge(..., all=TRUE), 
                        list(mean1yr, mean2yr, mean3yr, mean4yr, mean5yr, mean6yr, mean7yr))
  
  
write.csv(temp_history, "climate_data/max_temp_history.csv", row.names = FALSE)  
  
  
  
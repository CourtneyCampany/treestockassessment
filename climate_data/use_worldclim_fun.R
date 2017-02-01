
source("climate_data/worldclim_functions.R")

library(raster)
library(rgdal)

# download rasters once for a session
# or just once; specify a cache folder and it won't download again
wc <- get_worldclim_rasters(topath=tempdir())

# first argument is dataframe with longitude, latitude
# obs <- data.frame(latitude=c(-33, -34), longitude=c(150,150.1))

nursery <- read.csv("climate_data/nursery_locations.csv")

nursery_met <- get_worldclim_prectemp(nursery, worldclim=wc)
# tmean_1 is january, and so on
# MAT, MAP mean annual T and precip

write.csv(nursery_met, "climate_data/nursery_climate_history.csv", row.names = FALSE)

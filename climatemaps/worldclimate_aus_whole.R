##australia climate, using climate api

library("rWBclimate")

##maybe a map and temperature projections?


# Set the kmlpath option
options(kmlpath = "~/")
## Here we use a list basins for Africa
aus_basin <- create_map_df('AUS')

library(ggplot2)
ggplot(aus_basin, aes(x = long, y = lat, group = group)) + geom_polygon() + theme_bw()


aus_dat <- get_ensemble_temp('AUS', "annualanom", 2080, 2100)
## Subset data to just one scenario, and one percentile
aus_dat <- subset(aus_dat, aus_dat$scenario == "a2")
aus_dat <- subset(aus_dat, aus_dat$percentile == 50)

aus_map <- climate_map(aus_basin, aus_dat, return_map = T)
aus_map + scale_fill_continuous("Temperature \n anomaly", low = "yellow", high = "red") +
  theme_bw()


###climate variable--------------------------------------------------------------------------------------------------
aus.rain <- get_ensemble_precip("AUS", "mavg", 2020, 2040)
# Set line types
ltype <- rep(1, dim(aus.rain)[1])
ltype[aus.rain$percentile != 50] <- 2
aus.rain$ltype <- ltype

# Create uniqueIDs
aus.rain$uid <- paste(aus.rain$scenario, aus.rain$percentile, sep = "-")
ggplot(aus.rain, aes(x = as.factor(month), y = data, group = uid, colour = scenario,
                    linetype = as.factor(ltype))) + geom_point() + geom_path() + xlab("Month") +
                    ylab("Rain in mm") + theme_bw()



aus.temp <- get_model_temp("AUS", "mavg", 2020, 2040)
aus.temp.bcc <- aus.temp[aus.temp$gcm == "bccr_bcm2_0", ]
aus.temp.had <- aus.temp[aus.temp$gcm == "ukmo_hadcm3", ]
## Add a unique ID to each for easier plotting
aus.temp.bcc$ID <- paste(aus.temp.bcc$scenario, aus.temp.bcc$gcm, sep = "-")
aus.temp.had$ID <- paste(aus.temp.had$scenario, aus.temp.had$gcm, sep = "-")
plot.df <- rbind(aus.temp.bcc, aus.temp.had)
ggplot(plot.df, aes(x = as.factor(month), y = data, group = ID, colour = gcm,
                    linetype = scenario)) + geom_point() + geom_path() + 
                    ylab("Average temperature between 2080 and 2100 (C)") +
                    xlab("Month") + theme_bw()


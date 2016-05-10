library(scales)
library(magicaxis)
library(RColorBrewer)
library(shiny)

standard <- read.csv("shiny_data/container_assessment.csv")
standard$logminSI <- with(standard, log10(min_size_index))
standard$logmaxSI <- with(standard, log10(max_size_index))
standard$logvol <- with(standard, log10(container_volume))

minx <- min(standard$logvol)
maxx <- max(standard$logvol)
minsimin <- min(standard$logminSI)
maxsimin <- max(standard$logminSI)
minsimax <- min(standard$logmaxSI)
maxsimax <- max(standard$logmaxSI)


alpine_dat <- read.csv("shiny_data/alpine_sizeindex.csv")
#format raw dataframe

##need to replace 'mm' plots with appropriate volume
alpine_dat$volume <- gsub("300mm", 15, alpine_dat$volume)
alpine_dat$volume <- gsub("400mm", 35, alpine_dat$volume)
alpine_dat$volume <- gsub("500mm", 65, alpine_dat$volume)
alpine_dat$volume <- as.numeric(alpine_dat$volume)

#units and date formatting
alpine_dat$height_m <- alpine_dat$height/100
alpine_dat$date <- as.Date(alpine_dat$date, format = "%d/%m/%Y", tz="AEST")

#calulate indices
alpine_dat$calliper300 <- with(alpine_dat, (diameter1+diameter2)/2)
alpine_dat$rcd <- with(alpine_dat, (rcd1+rcd2)/2)
alpine_dat$sizeindex <- with(alpine_dat, height_m * calliper300)
alpine_dat$slenderness1 <- with(alpine_dat, height_m/rcd)
alpine_dat$slenderness2 <- with(alpine_dat, height_m/calliper300)
#log data for plotting
alpine_dat$logSI <- with(alpine_dat, log10(sizeindex))
alpine_dat$logvol <- with(alpine_dat, log10(volume))
alpine_dat$logH <- with(alpine_dat, log10(height_m))
alpine_dat$logD <- with(alpine_dat, log10(calliper300))
alpine_dat$logRCD <- with(alpine_dat, log10(rcd))
alpine_dat$logslender <- with(alpine_dat, log10(slenderness2))

appdat <- alpine_dat[, c("logSI","logvol", "species")]
species <- as.character(unique(alpine_dat$species))
silab <- expression(Size~index~range~~(calliper~x~height))
greytrans <- scales::alpha("grey", alpha=0.25)
blacktrans <- scales::alpha("black", alpha=0.6)
###large color palette
# n <- 22
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
speciesnames <- unique(alpine_dat$species)
speciesnames2 <- data.frame(species = speciesnames, colorspec = col_vector[1:22])

redtrans <- scales::alpha("red4", alpha=0.5)
appdat <- merge(appdat, speciesnames2)





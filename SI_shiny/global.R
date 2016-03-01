library(shiny)
library(magicaxis)
library(scales)

##colors------------------------------------------------------------------------------------------------------
greytrans <- alpha("grey", alpha=0.25)
blacktrans <- alpha("black", alpha=0.6)

goldtrans <- alpha("gold", alpha=0.5)
redtrans <- alpha("red4", alpha=0.5)
greentrans <- alpha("green2", alpha=0.5)
bluetrans <- alpha("blue", alpha=0.5)

darkbluetrans <- alpha("bisque2", alpha=0.5)

darkgreentrans <- alpha("darkgreen", alpha=0.5)
browntrans <- alpha("chocolate4", alpha=0.5)

##readdata---------------------------------------------------------------------------------------------------------
prelim <- read.csv("data/nursery_data.csv")
prelim$SI <- with(prelim, height * caliper_30cm)
prelim$logSI <- with(prelim, log10(SI))
prelim$logvol <- with(prelim, log10(container_l))
prelim$d2h <- with(prelim, caliper_30cm^2 * height)

standard <- read.csv("data/si_standard.csv")
standard$logminSI <- with(standard, log10(min_size_index))
standard$logmaxSI <- with(standard, log10(max_size_index))
standard$logvol <- with(standard, log10(container_volume))


#nursery datat
appdata <- prelim[, c(1,8:10,12:13) ] 
##reorder for shiny
appdata2 <- appdata[, c(5:6,1:4)]
appdata2$origin <- gsub("-", "", appdata2$origin)

##need to assign colors for factors
appdata2$colorgrow <- sapply(appdata2$growth_rate, function(x) switch(as.character(x),
                      fast=greentrans, moderate=goldtrans, slow=redtrans))

appdata2$colororigin <- sapply(appdata2$origin, function(x) switch(as.character(x),
                         native=blacktrans, nonnative=darkbluetrans))

appdata2$colorleaf <- sapply(appdata2$type, function(x) switch(as.character(x),
                        evergreen=darkgreentrans, deciduous=browntrans))


##plotbits-------------------------------------------------------------------------------------------------------------------
silab <- expression(Size~index~range~~(calliper~x~height)) 
cols <- c("forestgreen", "gold", "red4")

minx <- min(standard$logvol)
maxx <- max(standard$logvol)
minsimin <- min(standard$logminSI)
maxsimin <- max(standard$logminSI)
minsimax <- min(standard$logmaxSI)
maxsimax <- max(standard$logmaxSI)





library(doBy)
library(ggplot2)
library(magicaxis)
library(RColorBrewer)
library(scales)

# read data ---------------------------------------------------------------

si_means_clim <- read.csv("master_scripts/si_means_climate.csv") 
si_clim <- read.csv("master_scripts/si_climate.csv")
standard <- read.csv("reports/container_assessment.csv")

##drop small containers (5L and 14L)
si_range <- si_clim[si_clim$volume >= 18,]
si_means_range <- si_means_clim[si_means_clim$volume >= 18,]

# plot bits ---------------------------------------------------------------


silab <- expression(Size~index~range~~(calliper~x~height))

palette(c(alpha("forestgreen", .4), alpha("goldenrod1",.4), alpha("navyblue", .4), alpha("firebrick2", .4),
          alpha("darkorchid3", .4), alpha("deepskyblue1", .4)))

crlab <- as.vector(unique(si_means_clim$climate_region))
transcol <- alpha("firebrick1", alpha=.4)

# plotting ----------------------------------------------------------------

##which are the most frequent used sizes?
table(si_means_range$volume)
##20, 45, 100, 200, 400, 1000


##20 liter

hist(si_range[si_range$volum ==20,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,250))
box()
rect(24,0,37,600, col=transcol, border=NA)
legend("topright", "Container Volume = 20 L",bty='n', inset=.01)

##45 liter

hist(si_range[si_range$volum ==45,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,1200))
box()
rect(51,0,75,1500, col=transcol, border=NA)
legend("topright", "Container Volume = 45 L",bty='n', inset=.01)

##100 liter

hist(si_range[si_range$volum ==100,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,500))
box()
rect(102,0,150,600, border=NA, col=transcol)
legend("topright", "Container Volume = 100 L",bty='n', inset=.01)

##200 liter

hist(si_range[si_range$volum ==200,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,250))
box()
rect(185,0,272,300, border=NA, col=transcol)
legend("topright", "Container Volume = 200 L",bty='n', inset=.01)

##400 liter

hist(si_range[si_range$volum ==400,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,75))
box()
rect(330,0,494,100, border=NA, col=transcol)
legend("topright", "Container Volume = 400 L",bty='n', inset=.01)

##1000 liter

hist(si_range[si_range$volum ==1000,"sizeindex"], breaks=15, xlab = "sizeindex", main="", ylim=c(0,30))
box()
rect(739,0,1087,50, border=NA, col=transcol)
legend("topleft", "Container Volume = 1000 L",bty='n', inset=.001)


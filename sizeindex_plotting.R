library(doBy)
library(ggplot2)
library(magicaxis)
library(RColorBrewer)
library(scales)

# read data ---------------------------------------------------------------
variety <- read.csv("calculated_data/species_list.csv")
genusspecies <- read.csv("calculated_data/genusspecies_list.csv")
si_means_clim <- read.csv("master_scripts/si_means_climate.csv") 
si_clim <- read.csv("master_scripts/si_climate.csv")
standard <- read.csv("reports/container_assessment.csv")


# size index plot bits -----------------------------------------------------

silab <- expression(Size~index~range~~(calliper~x~height))

palette(c(alpha("forestgreen", .4), alpha("goldenrod1",.4), alpha("navyblue", .4), alpha("firebrick2", .4),
          alpha("darkorchid3", .4), alpha("deepskyblue1", .4)))

crlab <- as.vector(unique(si_means_clim$climate_region))

# trim data so AS2303 range --------------------------------------------------------------
si_range <- si_clim[si_clim$volume >= 18,]
si_mean_range <- si_means_clim[si_means_clim$volume >= 18,]

# plot SI data -----------------------------------------------------------

##means
windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(log10(sizeindex.mean) ~ log10(volume), data=si_mean_range, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7),ylim=c(0.3,3.7),
     axes=FALSE, cex=1, col=climate_region, pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
legend(x=2.95, y=1.35 , crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

box()


##all data
windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))

plot(logSI ~ logvol, data=oz_si_range, xlab="Container volume (L)", 
     ylab=silab, xlim=c(1,3.7),ylim=c(0.3,3.7),
     axes=FALSE, cex=.6, col=climate_region, pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
legend(x=2.95, y=1.35 , crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

box()


# variation plotting ------------------------------------------------------

plot(volume ~ sizeindex.mean, data=si_mean_range, ylab="Container volume (L)", xlab = silab, type='n')
segments(x0=23.8, y0=100, x1=346.4, y1=100, col="royalblue", lwd=2)
segments(x0=96.36, y0=1000, x1=1239.1, y1=1000, col="royalblue", lwd=2)
segments(x0=6.52, y0=20, x1=117.36, y1=20, col="royalblue", lwd=2)
segments(x0=12.09, y0=45, x1=12.09, y1=45, col="royalblue", lwd=2)
segments(x0=98.69845, y0=400, x1=12.09, y1=400, col="royalblue", lwd=2)
segments(x0=654.003, y0=1500, x1=12.09, y1=1500, col="royalblue", lwd=2)
segments(x0=921.8318, y0=2500, x1=1690.2100, y1=2500, col="royalblue", lwd=2)
segments(x0=255.9656, y0=75, x1=909.0930, y1=75, col="royalblue", lwd=2)

###make this a function/loop
for(i in unique(si_range$volume)){
  range(si_range[si_range$volume == 750, "sizeindex"])
}

lines(container_volume[1:36] ~ min_size_index[1:36], data=standard,lwd=1, col="grey")
lines(container_volume[1:36] ~ max_size_index[1:36], data=standard,lwd=1, col="grey")

# plot hieght data -----------------------------------------------------------

##means
windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(log10(height_m.mean) ~ log10(volume), data=si_mean_range, xlab="Container volume (L)", 
     ylab="Hieght (m)", xlim=c(1,3.7),ylim=c(0,1),
     axes=FALSE, cex=1, col=climate_region, pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

legend("bottomright", crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

box()


##all data
windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))

plot(logH ~ logvol, data=oz_si_range, xlab="Container volume (L)", 
     ylab="Hieght (m)", xlim=c(1,3.7),ylim=c(-.3,1),
     axes=FALSE, cex=.6, col=climate_region, pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

legend("bottomright", crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

box()


# plot diameter data -----------------------------------------------------------

##means
windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(log10(calliper300.mean) ~ log10(volume), data=si_mean_range, xlab="Container volume (L)", 
     ylab="Diameter @ 30cm (mm)", xlim=c(1,3.7),ylim=c(.75,2.5),
     axes=FALSE, cex=1, col=climate_region, pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

legend("bottomright", crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

box()


##all data
windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))

plot(logD ~ logvol, data=oz_si_range, xlab="Container volume (L)", 
     ylab="Diameter @ 30cm (mm)", xlim=c(1,3.7),ylim=c(.75,2.75),
     axes=FALSE, cex=.6, col=climate_region, pch=16)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

legend("bottomright", crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

box()

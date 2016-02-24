source("functions_and_packages/plot_objects.R")


##Test size index with prelim data


##standard
standard <- read.csv("data/container_assessment.csv")

###nursery data
prelim <- read.csv("data/prelim_data.csv")
  prelim$SI <- with(prelim, height * caliper_30cm)
  
  prelim$logSI <- with(prelim, log10(SI))
  prelim$logvol <- with(prelim, log10(container_l))
  prelim$d2h <- with(prelim, caliper_30cm^2 * height)


cols <- c("green", "yellow", "red")
   
###plotting-------------------------------------------------------------------------------------------------------------
library(magicaxis)

windows(7,7)

par(mar=c(6,7,2,2),cex.axis=1.4, cex.lab=1.75,las=0,mgp=c(4.5,1,0))
plot(logSI ~ logvol, data=prelim, xlab="Container volume (L)", ylab=silab,
     axes=FALSE, cex=1.25, col=cols[prelim$growth_rate],xlim=c(1,3.5),ylim=c(1, 3.5), pch=16)

magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)


#add assessment
points(log10(min_size_index)~log10(container_volume), data=standard, col="black",  cex=1.25, type='l', lwd=3)
points(log10(max_size_index)~log10(container_volume), data=standard, col="black",  cex=1.25, type='l', lwd=3)

legend("topleft", c("Max. size index", "Min. size index") ,pch=c(16, 21), cex=1.25, bty='n', inset=.01)

box()

dev.off()

###same plot without log10
windows(7,7)

par(mar=c(6,7,2,2),cex.axis=.8, cex.lab=1,las=0,mgp=c(4.5,1,0))
plot(SI ~ container_l, data=prelim, xlab="Container volume (L)", ylab=silab,cex=1.25, col=cols[prelim$type], pch=17,
     xlim=c(0,1000), ylim=c(0,1000))

#add assessment
points(min_size_index~container_volume, data=standard, col="black",  cex=1.25, type='l', lwd=3)
points(max_size_index~container_volume, data=standard, col="black",  cex=1.25, type='l', lwd=3)

legend("topleft", c("Max. size index", "Min. size index") ,pch=c(16, 21), cex=1, bty='n', inset=.01)

box()

dev.off()




###d2h figure
windows(7,7)

par(mar=c(6,7,2,2),cex.axis=.8, cex.lab=1,las=0,mgp=c(4.5,1,0))
plot(log10(d2h) ~ log10(container_l), data=prelim, xlab="Container volume (L)", ylab="Stem Volume",cex=1.25, 
     axes=FALSE,col=species, pch=16)
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

box()

dev.off()

###diameter
###d2h figure
windows(7,7)

par(mar=c(6,7,2,2),cex.axis=.8, cex.lab=1,las=0,mgp=c(4.5,1,0))
plot(log10(caliper_30cm) ~ log10(container_l), data=prelim, xlab="Caliper", ylab="Stem Volume",cex=1.25, 
     axes=FALSE,col=species, pch=16)
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

box()

dev.off()


source("functions_and_packages/plot_objects.R")


##Test size index with prelim data


##standard
standard <- read.csv("data/container_assessment.csv")

###nursery data
prelim <- read.csv("data/prelim_data.csv")
prelim$SI <- with(prelim, height * caliper_30cm)

##subset onlty species and SI
species_dat <- prelim[, c(1,3, 8)]
species_dat$logSI <- with(species_dat, log10(SI))
species_dat$logvol <- with(species_dat, log10(container_l))



###plotting-------------------------------------------------------------------------------------------------------------
library(magicaxis)

windows(7,7)

par(mar=c(6,7,2,2),cex.axis=1.4, cex.lab=1.75,las=0,mgp=c(4.5,1,0))
plot(logSI ~ logvol, data=species_dat, xlab="Container volume (L)", ylab=silab,
     axes=FALSE, cex=1.25, col=species,xlim=c(1,3.5),ylim=c(1, 3.5), pch=17)

magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)


#add assessment
points(log10(min_size_index)~log10(container_volume), data=standard, col="red",  cex=1.25, type='l', lwd=3)
points(log10(max_size_index)~log10(container_volume), data=standard, col="red",  cex=1.25, type='l', lwd=3)

legend("topleft", c("Max. size index", "Min. size index") ,pch=c(16, 21), cex=1.25, bty='n', inset=.01)

box()

dev.off()

###same plot without log10
windows(7,7)

par(mar=c(6,7,2,2),cex.axis=1.4, cex.lab=1.75,las=0,mgp=c(4.5,1,0))
plot(SI ~ container_l, data=species_dat, xlab="Container volume (L)", ylab=silab,cex=1.25, col=species, pch=17,
     xlim=c(0,1000), ylim=c(0,1000))

#add assessment
points(min_size_index~container_volume, data=standard, col="red",  cex=1.25, type='l', lwd=3)
points(max_size_index~container_volume, data=standard, col="red",  cex=1.25, type='l', lwd=3)

legend("topleft", c("Max. size index", "Min. size index") ,pch=c(16, 21), cex=1.25, bty='n', inset=.01)

box()

dev.off()

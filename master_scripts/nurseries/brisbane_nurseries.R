# read data ---------------------------------------------------------------

source("functions_and_packages/plot_objects.R")
source("functions_and_packages/size_index_format.R")

#standard
standard <- read.csv("reports/container_assessment.csv")

#nursery data (6total)

logans_dat <- read.csv("data/logans_sizeindex.csv")
pallara_dat <- read.csv("data/pallara_sizeindex.csv")
greenstock_dat <- read.csv("data/greenstock_sizeindex.csv")
ibrox_dat <- read.csv("data/ibrox_sizeindex.csv")
plantsdirect_dat <- read.csv("data/plantsdirect_sizeindex.csv")


logans <- melbs_format(logans_dat)
pallara <- melbs_format(pallara_dat)
greenstock <- melbs_format(greenstock_dat)
ibrox <- melbs_format(ibrox_dat)
plantsdirect <- melbs_format(plantsdirect_dat)

#save formatted perth data
write.csv(logans, "calculated_data/logans_clean.csv", row.names = FALSE)
write.csv(pallara, "calculated_data/pallara_clean.csv", row.names = FALSE)
write.csv(greenstock, "calculated_data/greenstock_clean.csv", row.names = FALSE)
write.csv(ibrox, "calculated_data/ibrox_clean.csv", row.names = FALSE)
write.csv(plantsdirect, "calculated_data/plantsdirect_clean.csv", row.names = FALSE)


# brisbane plotting to test for bad data -------------------------------------

library(magicaxis)
library(RColorBrewer)
library(scales)

##plotbits
halfblack <- alpha("darkblue", .25)
silab <- expression(Size~index~range~~(calliper~x~height))

windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=greenstock, xlab="Container volume (L)", ylab=silab,   xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, col=as.factor(species),pch=1)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)

box()
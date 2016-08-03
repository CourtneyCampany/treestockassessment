source("functions_and_packages/plot_objects.R")
source("functions_and_packages/size_index_format.R")

##standard
standard <- read.csv("reports/container_assessment.csv")
##nursery data
st_dat <- read.csv("data/speciality_sizeindex.csv")
mtwilly_dat <- read.csv("data/mtwilly_sizeindex.csv")
flem_dat <- read.csv("data/flemings_sizeindex.csv")
ett_dat <- read.csv("data/ett_sizeindex.csv")

##format data-

speciality <- melbs_format(st_dat)
mywilly <- melbs_format(mtwilly_dat)
ett <- melbs_format(ett_dat)
fleming <- melbs_format(flem_dat)

#size index plotting--------------------------------------------------------------------------------------------------------

library(magicaxis)
library(RColorBrewer)

##plotbits
silab <- expression(Size~index~range~~(calliper~x~height))

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=speciality, xlab="Container volume (L)", ylab=silab,   xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, bg="red",pch=21)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

points(logSI ~ logvol, data=mywilly, cex=1.25, bg="blue",pch=21)
points(logSI ~ logvol, data=ett, cex=1.25, bg="forestgreen",pch=21)
points(logSI ~ logvol, data=fleming, cex=1.25, bg="gold",pch=21)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)

box()
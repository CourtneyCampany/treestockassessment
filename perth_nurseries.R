source("functions_and_packages/plot_objects.R")
source("functions_and_packages/size_index_format.R")

##standard
standard <- read.csv("reports/container_assessment.csv")
##nursery data
dph_dat <- read.csv("data/darwin_sizeindex.csv")


##format data-
fres <- darwin_format(dph_dat)
#save formatted darwin data
write.csv(darwin, "calculated_data/darwin_clean.csv", row.names = FALSE)

length(unique(darwin$species))

#size index plotting--------------------------------------------------------------------------------------------------------

library(magicaxis)
library(RColorBrewer)
library(scales)


##plotbits
halfblack <- alpha("darkblue", .25)
silab <- expression(Size~index~range~~(calliper~x~height))

windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=darwin, xlab="Container volume (L)", ylab=silab,   xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, col=halfblack,pch=1)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
title(main="Darwin Plant Wholesalers")
box()
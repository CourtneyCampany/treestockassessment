# read data ---------------------------------------------------------------
source("functions_and_packages/plot_objects.R")
source("functions_and_packages/size_index_format.R")

#standard
standard <- read.csv("reports/container_assessment.csv")

#nursery data from other NSW nurseries

impact_dat <- read.csv("data/treesimpact_sizeindex.csv")

# format and write data ---------------------------------------------------
treesimpact <- melbs_format(impact_dat)

#save formatted nsw data
write.csv(treesimpact, "calculated_data/treesimpact_clean.csv", row.names = FALSE)

length(unique(treesimpact$species))
which(is.na(treesimpact$sizeindex))

#plotting to test for bad data -------------------------------------

library(magicaxis)
library(RColorBrewer)
library(scales)

##plotbits
halfblack <- alpha("darkblue", .25)
silab <- expression(Size~index~range~~(calliper~x~height))

windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=treesimpact, xlab="Container volume (L)", ylab=silab,   xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, col=halfblack,pch=1)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)

box()
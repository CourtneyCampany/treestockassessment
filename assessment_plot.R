#mage conceptual figures of assessment

#read container data

pots <- read.csv("data/container_assessment.csv")
##calculate the midpoint, which we will assume is the ideal size 
pots$si_mid <- with(pots, ((max_size_index - min_size_index)/2)+min_size_index)

windows(10,6)
par(las=1,mgp=c(3,1,0),mfrow=c(1,2), mar=c(5,5,1,1))


plot(container_volume ~ min_size_index, data=pots, col="forestgreen", pch=16, cex=1.25,
     xlab="Tree Size Index", ylab="")
points(container_volume ~ max_size_index, data=pots, col="blue", pch=16, cex=1.25)
points(container_volume ~ si_mid , data=pots, col="black", type='l', lwd=2, lty=2)
title(ylab = "Container Volume (l)", mgp = c(3.5, 1, 0))

##log
library(magicaxis)

plot(log10(container_volume) ~ log10(min_size_index), data=pots, col="forestgreen", 
     pch=16, cex=1.25,axes=FALSE, xlab="log(Tree Size Index)", ylab="")
points(log10(container_volume) ~ log10(max_size_index), data=pots, col="blue", pch=16, cex=1.25)
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
title(ylab = "log(Container Volume) (l)", mgp = c(3.5, 1, 0))


###bareroot

bareroot <- read.csv("data/exground_assessment.csv")
##calculate the midpoint, which we will assume is the ideal size 
bareroot$si_mid <- with(bareroot, ((max_size_index - min_size_index)/2)+min_size_index)

windows(10,6)
par(las=1,mgp=c(3,1,0),mfrow=c(1,2), mar=c(5,5,1,1))


plot(rootball_diam_min ~ min_size_index, data=bareroot, col="forestgreen", pch=16, cex=1.25,
     xlab="Tree Size Index", ylab="")
points(rootball_diam_min ~ max_size_index, data=bareroot, col="blue", pch=16, cex=1.25)
points(rootball_diam_min ~ si_mid , data=bareroot, col="black", type='l', lwd=2, lty=2)
title(ylab = "Rootball Diameter (mm)", mgp = c(3.5, 1, 0))

##log
library(magicaxis)

plot(log10(rootball_diam_min) ~ log10(min_size_index), data=bareroot, col="forestgreen", 
     pch=16, cex=1.25,axes=FALSE, xlab="log(Tree Size Index)", ylab="")
points(log10(rootball_diam_min) ~ log10(max_size_index), data=bareroot, col="blue", pch=16, cex=1.25)
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
title(ylab = "log(Rootbal Diameter) (mm)", mgp = c(3.5, 1, 0))






windows(10,6)
par(las=1,mgp=c(3,1,0),mfrow=c(1,2), mar=c(5,5,1,1))


plot(min_size_index~container_volume, data=pots, col="forestgreen", pch=16, cex=1.25,
     xlab="Container Volume (l)", ylab="", ylim=c(0,2500), xlim=c(0,2500))
points(max_size_index~container_volume , data=pots, col="blue", pch=16, cex=1.25)
points(si_mid~container_volume , data=pots, col="black", type='l', lwd=2, lty=2)
title(ylab = "Tree Size Index", mgp = c(3.5, 1, 0))

##log
library(magicaxis)

plot(log10(min_size_index)~log10(container_volume) , data=pots, col="forestgreen", 
     pch=16, cex=1.25,axes=FALSE, xlab="log(Container Volume) (l)", ylab="")
points(log10(max_size_index)~log10(container_volume), data=pots, col="blue", pch=16, cex=1.25)
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
title(ylab = "log(Tree Size Index) (l)", mgp = c(3.5, 1, 0))

###use that perfect number to predict, where does 95CI sit? Is it outside of the min and max?



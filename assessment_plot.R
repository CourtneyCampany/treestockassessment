#mage conceptual figures of assessment

#read container data

pots <- read.csv("data/container_assessment.csv")
##calculate the midpoint, which we will assume is the ideal size 
pots$si_mid <- with(pots, ((max_size_index - min_size_index)/2)+min_size_index)

##lets create some fake CIs for effect, showing where data may lie???
pots$si_mid25upr <- with(pots, si_mid+(si_mid*.25)) 
pots$si_mid25lwr <- with(pots, si_mid-(si_mid*.25)) 

pots$si_mid50upr <- with(pots, si_mid+(si_mid*.5)) 
pots$si_mid50lwr <- with(pots, si_mid-(si_mid*.5)) 


windows(10,6)
par(las=1,mgp=c(3,1,0),mfrow=c(1,2), mar=c(5,5,1,1))


plot(min_size_index~container_volume, data=pots, col="forestgreen", pch=16, cex=1.25,
     xlab="Container Volume (l)", ylab="", ylim=c(0,2500), xlim=c(0,2500))
points(max_size_index~container_volume , data=pots, col="blue", pch=16, cex=1.25)
points(si_mid~container_volume , data=pots, col="black", type='l', lwd=2, lty=2)

lines(pots$container_volume, pots$si_mid25upr, lty=2, lwd=2,col="grey")
lines(pots$container_volume, pots$si_mid25lwr, lty=2, lwd=2,col="grey")

lines(pots$container_volume, pots$si_mid50upr, lty=2, lwd=2,col="orange")
lines(pots$container_volume, pots$si_mid50lwr, lty=2, lwd=2,col="orange")


title(ylab = "Tree Size Index", mgp = c(3.5, 1, 0))

##log
library(magicaxis)

plot(log10(min_size_index)~log10(container_volume) , data=pots, col="forestgreen", 
     pch=16, cex=1.25,axes=FALSE, xlab="Container Volume (l)", ylab="", xlim=c(1,3.5), ylim=c(1,3.5))
points(log10(max_size_index)~log10(container_volume), data=pots, col="blue", pch=16, cex=1.25)
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
title(ylab = "Tree Size Index (l)", mgp = c(3.5, 1, 0))

###use that perfect number to predict, where does 95CI sit? Is it outside of the min and max?




###bareroot--------------------------------------------------------------------------------------

bareroot <- read.csv("data/exground_assessment.csv")
##calculate the midpoint, which we will assume is the ideal size 
bareroot$si_mid <- with(bareroot, ((max_size_index - min_size_index)/2)+min_size_index)

windows(10,6)
par(las=1,mgp=c(3,1,0),mfrow=c(1,2), mar=c(5,5,1,1))


plot(min_size_index~ rootball_diam_min, data=bareroot, col="forestgreen", pch=16, cex=1.25,
     xlab="Rootball Diameter (mm)", ylab="", ylim=c(0,3500))
points(max_size_index ~ rootball_diam_min , data=bareroot, col="blue", pch=16, cex=1.25)
points(si_mid~ rootball_diam_min  , data=bareroot, col="black", type='l', lwd=2, lty=2)
title(ylab = "Tree Size Index (calliper x height)", mgp = c(3.5, 1, 0))

##log
library(magicaxis)

plot(log10(min_size_index)~log10(rootball_diam_min), data=bareroot, col="forestgreen", 
     pch=16, cex=1.25,axes=FALSE, xlab="Rootball Diameter (mm)", ylab="", xlim=c(1,3.5), ylim=c(1,3.5))
points(log10(max_size_index)~log10(rootball_diam_min), data=bareroot, col="blue", pch=16, cex=1.25)
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
title(ylab = "Tree Size Index (calliper x height)", mgp = c(3.5, 1, 0))










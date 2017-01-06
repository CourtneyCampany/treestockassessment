source("plot_objects.R")

### Size Index for all nurseries
oz_sizeindex <- read.csv("calculated_data/oz_sizeindex.csv")

##standard
standard <- read.csv("container_assessment.csv")

#size index plotting--------------------------------------------------------------------------------------------------------

library(magicaxis)
library(RColorBrewer)
library(scales)

##plotbits
halfblack <- alpha("darkblue", .25)
silab <- expression(Size~index~range~~(calliper~x~height))

###nice color palette
cols <-  brewer.pal(6, "Set1")
cols2 <- c(alpha(cols[5], .65), alpha(cols[3], .65), alpha(cols[2], .65), alpha(cols[1], .65))




windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ jitter(logvol,5), data=oz_sizeindex, xlab="Container volume (L)", ylab=silab,   xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, bg=cols2[oz_sizeindex$climate_region],pch=21)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2.5)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2.5)


tb <- do.call(rbind, with(oz_sizeindex, tapply(sizeindex, volume, range)))
plover:::addpoly(log10(as.numeric(rownames(tb))), log10(tb[,1]), log10(tb[,2]))


legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
legend("bottomright", c("New South Wales", "Northern Territory", "Perth", "Victoria"), pch=21, pt.bg =cols2, inset=.025)
box()


addpoly <- function(x,y1,y2,col=alpha("lightgrey",0.8),border=NA,...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=border,...)
}

library(doBy)
oz_sizeindex_a <- summaryBy(. ~ batch_id2, FUN=mean, data=oz_sizeindex, keep.names=TRUE,
                            id=~climate_region+species)


windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=oz_sizeindex_a, xlab="Container volume (L)", ylab=silab,   
     xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, bg=cols2[oz_sizeindex_a$climate_region],pch=21)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2.5)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2.5)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
legend("bottomright", c("New South Wales", "Northern Territory", "Perth", "Victoria"), pch=21, pt.bg =cols2, inset=.025)
box()
addpoly(log10(as.numeric(rownames(tb))), log10(tb[,1]), log10(tb[,2]), col=NA, border="darkgrey")


pot100 <- subset(oz_sizeindex_a, volume == 100)


oz_sizeindex$predsizeindex <- 0.452 + 0.861 * oz_sizeindex$volume


# github.com/remkoduursma/plover
library(plover)

hist_bygroup(sizeindex-predsizeindex, climate_region, data=oz_sizeindex, breaks=300, 
             col=cols2, what="density", xlim=c(-200,200))


trees100 <- subset(oz_sizeindex,volume==100)
hist_bygroup(sizeindex-predsizeindex, climate_region, dataset=trees100, breaks=25, col=cols2, what="density", xlim=c(-200,200))

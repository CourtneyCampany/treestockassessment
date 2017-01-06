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
   
###large color palette
library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))



###plotting-------------------------------------------------------------------------------------------------------------
library(magicaxis)

windows(7,7)
png(filename = "images/prelimdata.png", width = 5, height = 5, units = "in", res= 600)
par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=prelim, xlab="Container volume (L)", ylab=silab,
     axes=FALSE, cex=1.25, bg=col_vector[prelim$species],xlim=c(1,3),ylim=c(1, 3), pch=21)

magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
points(log10(min_size_index[1:30])~log10(container_volume[1:30]), data=standard, bg="grey65",   type='l',lwd=2)
points(log10(max_size_index[1:30])~log10(container_volume[1:30]), data=standard, col="black",  type='l', lwd=2)

# legend("topleft", c("Max. size index", "Min. size index") ,pch=c(16, 21), cex=1.25, bty='n', inset=.01)

box()

# dev.copy2pdf(file= "images/prelimdata.pdf")
dev.off()


####make loglog plot of SI and prelim data------------------------------------------------------------------------------------

par(mar=c(6,7,2,2),cex.axis=1.4, cex.lab=1.75,las=0,mgp=c(4.5,1,0))
plot(logSI ~ logvol, data=pots, xlab="Container volume (L)", ylab=expression(Size~index~range~~(calliper~x~height)),
     axes=FALSE, cex=1.25, col=unordercols,xlim=c(.05,3.4),ylim=c(0.05, 3.5))
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
points(logSI~ log10(volume), data=pots_final, col=potcols, pch=16, cex=1.6)

#add assessment
points(log10(min_size_index)~log10(container_volume), data=assess, col="black", pch=21, cex=1.25)
points(log10(max_size_index)~log10(container_volume), data=assess, col="black", pch=16, cex=1.25)

legend("topleft", c("Max. size index", "Min. size index") ,pch=c(16, 21), cex=1.25, bty='n', inset=.01)


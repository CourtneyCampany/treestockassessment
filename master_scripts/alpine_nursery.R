source("functions_and_packages/plot_objects.R")

##standard
standard <- read.csv("data/container_assessment.csv")
alpine_dat <- read.csv("data/alpine_sizeindex.csv")


#function to calculate size index, etc from nursery data---------------

#format raw dataframe

##need to replace 'mm' plots with appropriate volume
alpine_dat$volume <- gsub("300mm", 15, alpine_dat$volume)
alpine_dat$volume <- gsub("400mm", 35, alpine_dat$volume)
alpine_dat$volume <- gsub("500mm", 65, alpine_dat$volume)
alpine_dat$volume <- as.numeric(alpine_dat$volume)
print("all container volumes corrected")

#units and date formatting
alpine_dat$height_m <- alpine_dat$height/100
alpine_dat$date <- as.Date(alpine_dat$date, format = "%d/%m/%Y", tz="AEST")
print("date conversion worked")

#deal with species name
#check data quality

#calulate indices
alpine_dat$calliper300 <- with(alpine_dat, (diameter1+diameter2)/2)
alpine_dat$rcd <- with(alpine_dat, (rcd1+rcd2)/2)

alpine_dat$sizeindex <- with(alpine_dat, height_m * calliper300)
alpine_dat$slenderness1 <- with(alpine_dat, height_m/rcd)
alpine_dat$slenderness2 <- with(alpine_dat, height_m/calliper300)

#log data for plotting
alpine_dat$logSI <- with(alpine_dat, log10(sizeindex))
alpine_dat$logvol <- with(alpine_dat, log10(volume))
alpine_dat$logH <- with(alpine_dat, log10(height_m))
alpine_dat$logD <- with(alpine_dat, log10(calliper300))
alpine_dat$logRCD <- with(alpine_dat, log10(rcd))
alpine_dat$logslender <- with(alpine_dat, log10(slenderness2))

#total number of observations
print(paste(nrow(alpine_dat), "trees measured"))


alpine_agg <- doBy::summaryBy(sizeindex+slenderness2 ~ species+batch_id, fun=mean, data=alpine_dat, keep.names = TRUE)
#number of batches?
length(unique(alpine_agg$batch_id))
#number of species?
length(unique(alpine_agg$species))
#number of container sizes?
length(unique(alpine_dat$volume))


###function for percent data within size index range



###plotting-------------------------------------------------------------------------------------------------------------
library(magicaxis)
library(RColorBrewer)

##plotbits
###large color palette
n <- 30
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))


#1. size index vs volume
windows(7,7)
#png(filename = "images/prelimdata.png", width = 5, height = 5, units = "in", res= 600)
par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=alpine_dat, xlab="Container volume (L)", ylab=silab,
     axes=FALSE, cex=1.25, bg=col_vector[alpine_dat$species],pch=21, xlim=c(.5,3.75), ylim=c(.5,3.5))
magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
points(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard, bg="grey65",   type='l',lwd=2)
points(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard, col="black",  type='l', lwd=2)

legend("topleft", c("Max. size index", "Min. size index") ,pch=c(16, 21), cex=1.25, bty='n', inset=.01)

box()



#2.  height vs volume
windows(7,7)

par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logH ~ logvol, data=alpine_dat, xlab="Container volume (L)", ylab="Height (m)",
     axes=FALSE, cex=1.25, bg=col_vector[alpine_dat$species],pch=21, xlim=c(.5,3.75), ylim=c(-.5,1))
magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

box()

#3.  calliper vs volume
windows(7,7)

par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logD ~ logvol, data=alpine_dat, xlab="Container volume (L)", ylab="Stem Calliper @ 300mm (mm)",
     axes=FALSE, cex=1.25, bg=col_vector[alpine_dat$species],pch=21, xlim=c(.5,3.75), ylim=c(.5,2.25))
magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

box()

#4.  rcd vs volume
windows(7,7)

par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logRCD ~ logvol, data=alpine_dat, xlab="Container volume (L)", ylab="Root Collar Diameter (mm)",
     axes=FALSE, cex=1.25, bg=col_vector[alpine_dat$species],pch=21, xlim=c(1,3.75), ylim=c(1,2.25))
magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

box()

#4. slenderness index

windows(7,7)

par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logslender ~ logvol, data=alpine_dat, xlab="Container volume (L)", ylab="Slenderness Index (Height:Calliper)",
     axes=FALSE, cex=1.25, bg=col_vector[alpine_dat$species],pch=21, xlim=c(.5,3.75), ylim=c(-1.5, -.75))
magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

box()


#5. height vs diameter

windows(7,7)

par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(calliper300 ~ height_m, data=alpine_dat, xlab="Height (m)", ylab="Diameter (mm)",xlim=c(0,8), ylim=c(0,175),
     cex=1.25, col=col_vector[alpine_dat$species],pch=1)

points(rcd ~ height_m, data=alpine_dat, cex=1.25, bg=col_vector[alpine_dat$species],pch=21)
box()

legend("topleft", c("Root Collar Diameter","Calliper @ 300mm") ,pch=c(16, 1), bg="black", cex=1, bty='n', inset=.01)



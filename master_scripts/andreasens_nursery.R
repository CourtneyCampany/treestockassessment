source("functions_and_packages/plot_objects.R")

##standard
standard <- read.csv("reports/container_assessment.csv")
kemps_dat <- read.csv("data/kempscreek_sizeindex.csv")
  kemps_dat$site <- "Kemps Creek"
mangrove_dat <- read.csv("data/mangrovemtn_sizeindex.csv")
  mangrove_dat$site <- "Mangrove Mountain"
  
#need to seperate the trees with diameter tape and calculate size index
mangrove_big <- mangrove_dat[is.na(mangrove_dat$diameter2),]
mangrove_small <-  mangrove_dat[!is.na(mangrove_dat$diameter2),] 


#function for andreasens nurseries--------------------------------------------------------

andreasensformat <- function (x){
  
  x$date <- as.Date(x$date, format = "%m/%d/%Y", tz="AEST")
  print("date conversion worked")

  #need new id at Andreseans (batch_id may duplicate across species)
  x$batch_id2 <- paste(x$batch_id, x$species, sep="-")
  #calulate indices
  x$calliper300 <- with(x, (diameter1+diameter2)/2)
  x$rcd <- with(x, (rcd1+rcd2)/2)
  x$height_m <- x$height/100
  x$sizeindex <- with(x, height_m * calliper300)
  print("date format worked")
  
  #log data for plotting
  x$logSI <- with(x, log10(sizeindex))
  x$logvol <- with(x, log10(volume))
  x$logH <- with(x, log10(height_m))
  x$logD <- with(x, log10(calliper300))
  x$logRCD <- with(x, log10(rcd))
  print("log conversion worked")
  x2 <- x[, c("date", "species","site", "batch_id2","volume", "calliper300","rcd", "height_m", "sizeindex", "logSI", "logvol", 
              "logH", "logD", "logRCD")]
  return(x2)
}  


#function for mangrove mountain where diameter tape was used instead of callipers-------------------------------------------
mangrovebig_format <- function(x){
  x$date <- as.Date(x$date, format = "%m/%d/%Y", tz="AEST")
  print("date conversion worked")
  
  #need new id at Andreseans (batch_id may duplicate across species)
  x$batch_id2 <- paste(x$batch_id, x$species, sep="-")
  #calulate indices
  x$calliper300 <- with(x, (diameter1/pi)*10)
  x$rcd <- with(x, (rcd1/pi)*10)
  x$height_m <- x$height/100
  x$sizeindex <- with(x, height_m * calliper300)
  print("date format worked")
  
  #log data for plotting
  x$logSI <- with(x, log10(sizeindex))
  x$logvol <- with(x, log10(volume))
  x$logH <- with(x, log10(height_m))
  x$logD <- with(x, log10(calliper300))
  x$logRCD <- with(x, log10(rcd))
  print("log conversion worked")
  x2 <- x[, c("date", "species","site","batch_id2","volume","calliper300","rcd", "height_m", "sizeindex", "logSI", "logvol", 
              "logH", "logD", "logRCD")]
  return(x2)
}
 
#format data (merge mangrove datasets)
 
mangrove_small2 <- andreasensformat(mangrove_small)
mangrove_big2 <- mangrovebig_format(mangrove_big)

mangrove_si <- rbind(mangrove_small2, mangrove_big2)
kemps_si <- andreasensformat(kemps_dat)

###means
mangrove_agg <- doBy::summaryBy(sizeindex ~ species+batch_id2, fun=mean, data=mangrove_si, keep.names = TRUE)
mangrove_vols <- doBy::summaryBy(sizeindex ~ species+volume, fun=mean, data=kemps_si, keep.names = TRUE)
  
  
###plotting-------------------------------------------------------------------------------------------------------------
library(magicaxis)
library(RColorBrewer)
  
###large color palette
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

#colors for species
# mangrovespecies <- unique(mangrove_si$species)
# mangrovespecies2 <- data.frame(species = mangrovespecies, colorspec = col_vector[1:9])
# mangrove_si <- merge(mangrove_si, mangrovespecies2)
# 
# kempspecies <- unique(kemps_si$species)
# kempspecies2 <- data.frame(species = kempspecies, colorspec = col_vector[1:15])
# kemps_si <- merge(kemps_si, kempspecies2)

##Redo and combine both sites
andreasens_si <- rbind(kemps_si, mangrove_si)
andreasensspecies <- unique(andreasens_si$species)
andreasensspecies2 <- data.frame(species = andreasensspecies, colorspec = col_vector[1:20])
andreasens_si <- merge(andreasens_si, andreasensspecies2)

write.csv(andreasens_si,"reports/andreasens_sizeindex.csv", row.names = FALSE)
  
#1. size index vs volume
windows(7,7)
#png(filename = "images/prelimdata.png", width = 5, height = 5, units = "in", res= 600)
par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=andreasens_si, xlab="Container volume (L)", ylab=silab, xlim=c(0.5,3.5),ylim=c(0.3,3.5),
       axes=FALSE, cex=1.25, bg=col_vector[andreasens_si$species],pch=21)
  magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
  
  #add assessment
  lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
  lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
  
legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
  
box()
  
  
  
#2.  height vs volume
windows(7,7)
  
par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logH ~ logvol, data=andreasens_si, xlab="Container volume (L)", ylab="Height (m)",
       axes=FALSE, cex=1.25, bg=col_vector[andreasens_si$species],pch=21)
magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
  
box()
  
#3.  calliper vs volume
windows(7,7)
  
par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logD ~ logvol, data=andreasens_si, xlab="Container volume (L)", ylab="Stem Calliper @ 300mm (mm)",
       axes=FALSE, cex=1.25, bg=col_vector[andreasens_si$species],pch=21, xlim=c(.5,3.75), ylim=c(.5,2.25))
magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
  
box()
  
#4.  rcd vs volume
windows(7,7)
  
par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logRCD ~ logvol, data=andreasens_si, xlab="Container volume (L)", ylab="Root Collar Diameter (mm)",
       axes=FALSE, cex=1.25, bg=col_vector[andreasens_si$species],pch=21)
magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
  
box()
  

#5. height vs diameter
windows(7,7)
  
par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(calliper300 ~ height_m, data=andreasens_si, xlab="Height (m)", ylab="Diameter (mm)",xlim=c(0,8), ylim=c(0,175),
       cex=1.25, col=col_vector[andreasens_si$species],pch=1)
  
points(rcd ~ height_m, data=andreasens_si, cex=1.25, bg=col_vector[andreasens_si$species],pch=21)
box()
  
legend("topleft", c("Root Collar Diameter","Calliper @ 300mm") ,pch=c(16, 1), bg="black", cex=1, bty='n', inset=.01)
  


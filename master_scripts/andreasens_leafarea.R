source("functions_and_packages/functions.R")

# leaf area at kemps and mangriove mtn

mangrove_leaf <- read.csv("data/mangrovemtn_shape.csv")
kemp_leaf <- read.csv("data/kempscreek_shape.csv")


##formatinf function to calculate leaf area from nursery raw data
sla_format <- function(x){
  
  #need new id at Andreseans (batch_id may duplicate across species)
  x$batch_id2 <- paste(x$batch_id, x$species, sep="-")
  
  nodeciduous <- x[!is.na(x$leafarea1),] #from deciduous trees with no leaves
  
  ##need to deal with the fact that some leaves could not be weighed seperate
  oneleaf <- nodeciduous[is.na(nodeciduous$leafmass3),]
  oneleaf$leafarea <- with(oneleaf, leafarea1+leafarea2+leafarea3)
  oneleaf$sla <- with(oneleaf, leafarea/leafmass1)
  
  alleaves <- nodeciduous[!is.na(nodeciduous$leafmass3),]
  alleaves$sla1 <- with(alleaves, leafarea1/leafmass1)
  alleaves$sla2 <- with(alleaves, leafarea2/leafmass2)
  alleaves$sla3 <- with(alleaves, leafarea3/leafmass3)
  
  alleaves$sla <- rowMeans(alleaves[, c("sla1", "sla2", "sla3")])
  
  alleaves2 <- alleaves[, c("batch_id2", "treenumb", "volume", "species", "sla")]
  oneleaf2 <- oneleaf[, c("batch_id2", "treenumb", "volume", "species", "sla")]
  
  sla_dat <- rbind(alleaves2, oneleaf2)
  return(sla_dat)
}

kemps_sla <- sla_format(kemp_leaf)
mangrove_sla <- sla_format(mangrove_leaf)

andreasens_sla <- rbind(kemps_sla, mangrove_sla)

##nice colors
library(RColorBrewer)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
andreasensspecies <- unique(andreasens_sla$species)
andreasensspecies2 <- data.frame(species = andreasensspecies, colorspec = col_vector[1:17])

andreasens_sla <- merge(andreasens_sla, andreasensspecies2)

###need to remove 2 bad jacranda 400L trees
andreasens_sla2 <- andreasens_sla[andreasens_sla$species != "jacaranda_mimosifolia",]
andreasens_sla2 <- droplevels(andreasens_sla2)

##plot------------------------------------------------------------------------------------------------------------------------

plot(sla~volume, data=andeasens_sla, bg=col_vector[andeasens_sla$species],pch=21)

library(magicaxis)

par(mar=c(5,5,1,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(log10(sla) ~ log10(volume), data=andreasens_sla, xlab="Container volume (L)", ylab="Specific Leaf Area",
     axes=FALSE, cex=1.25, bg=col_vector[andreasens_sla$species],pch=21)
magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
box()


library(sciplot)
labels <- unique(andreasens_sla2$species)

par(mar=c(10,6,1,1))
slafig <- bargraph.CI(species, sla, data=andreasens_sla2, ylim=c(0,125), col="black", xaxt="n", err.width = .02)
text(slafig$xvals, par("usr")[3]-3.75 , srt = 40, adj = 1,labels = labels, xpd = TRUE)
box()

windows(7,7)
par(mar=c(14,6,1,1))
bar(sla,species,andreasens_sla2,ylim=c(0,125), legend=FALSE, ylab="", xlab="", las=2)


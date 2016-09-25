source("functions_and_packages/plot_objects.R")
source("functions_and_packages/size_index_format.R")

### Size Index for all nurseries

##read data 
darwin<- read.csv("calculated_data/darwin_clean.csv")
ett<- read.csv("calculated_data/ett_clean.csv")
fleming<- read.csv("calculated_data/fleming_clean.csv")
kemps<- read.csv("calculated_data/kemps_clean.csv")
mangrove<- read.csv("calculated_data/mangrove_clean.csv")
mtwilly<- read.csv("calculated_data/mtwilly_clean.csv")
speciality<- read.csv("calculated_data/speciality_clean.csv")
ellenby<- read.csv("calculated_data/ellenby_clean.csv")

alpine<- read.csv("calculated_data/alpine_clean.csv")
  alpine$batch_id <- as.factor(alpine$batch_id)

##standard
standard <- read.csv("reports/container_assessment.csv")

##merge all data
oz_sizeindex <- rbind(darwin, ett)
oz_sizeindex <- rbind(oz_sizeindex, fleming)
oz_sizeindex <- rbind(oz_sizeindex, mtwilly)
oz_sizeindex <- rbind(oz_sizeindex, speciality)
oz_sizeindex <- rbind(oz_sizeindex, alpine)
oz_sizeindex <- rbind(oz_sizeindex, kemps[, !names(kemps) %in% "site"])
oz_sizeindex <- rbind(oz_sizeindex, mangrove[, !names(mangrove) %in% "site"])
oz_sizeindex <- rbind(oz_sizeindex, ellenby)

##add climate zone
oz_sizeindex <- add_campaign_region(oz_sizeindex)

#save masterfile of sizeindex data
write.csv(oz_sizeindex, "calculated_data/oz_sizeindex.csv", row.names = FALSE)


length(unique(oz_sizeindex$species))
length(unique(oz_sizeindex$volume))
range(oz_sizeindex$volume)


##Determine total amount of species that do not fit in as2303
oz_sizeindex <- doesfit_func(oz_sizeindex)
library(plyr)
count(oz_sizeindex, var="balanced")


#size index plotting--------------------------------------------------------------------------------------------------------

library(magicaxis)
library(RColorBrewer)
library(scales)
library(wesanderson)

##plotbits
halfblack <- alpha("darkblue", .25)
silab <- expression(Size~index~range~~(calliper~x~height))

###nice color palette
cols <-  brewer.pal(6, "Set1")
cols2 <- c(alpha(cols[5], .65), alpha(cols[3], .65), alpha(cols[2], .65), alpha(cols[1], .65))

windows(7,7)
# png(filename = "master_scripts/ozsizeindex.png", width = 5, height = 5, units = "in", res= 600)
par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=oz_sizeindex, xlab="Container volume (L)", ylab=silab,   xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, bg=cols2[oz_sizeindex$climate_region],pch=21)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2.5)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2.5)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
legend("bottomright", c("New South Wales", "Northern Territory", "Perth", "Victoria"), pch=21, pt.bg =cols2, inset=.025)
# title(main="Australia Tree Nurseries")
box()

#dev.copy2pdf(file="master_scripts/oz_sizeindex.pdf")
dev.off()

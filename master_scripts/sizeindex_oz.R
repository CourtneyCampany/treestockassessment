### Size Index for all nurseries

##read data 
darwin<- read.csv("calculated_data/darwin_clean.csv")
ett<- read.csv("calculated_data/ett_clean.csv")
fleming<- read.csv("calculated_data/fleming_clean.csv")
kemps<- read.csv("calculated_data/kemps_clean.csv")
mangrove<- read.csv("calculated_data/mangrove_clean.csv")
mtwilly<- read.csv("calculated_data/mtwilly_clean.csv")
speciality<- read.csv("calculated_data/speciality_clean.csv")

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

##add climate zone
add_campaign_region <- function(x){
  
  x$climate_region <-ifelse(x$nursery == "dph", "Northern Territory", "huh")
  x$climate_region <-ifelse(x$nursery == "alp" | x$nursery == "a_kc" | x$nursery == "a_mm", "New South Wales", x$climate_region)
  x$climate_region <-ifelse(x$nursery == "flem" | x$nursery == "spec" | x$nursery == "ett" | x$nursery == "mtwil", 
                            "Victoria", x$climate_region )
  x$climate_region <- as.factor(x$climate_region)
  return(x)
}

oz_sizeindex <- add_campaign_region(oz_sizeindex)

length(unique(oz_sizeindex$species))
length(unique(oz_sizeindex$volume))
range(oz_sizeindex$volume)


##Determine total amount of species that do not fit in as2303
doesfit_func <- function(x) {
  x$balanced <- ifelse(x$volume == 5 & x$sizeindex >=7.7 & x$sizeindex <=11.3, "pass", "fail")
  x$balanced <- ifelse(x$volume == 14 & x$sizeindex >=18.6 & x$sizeindex <=27.5, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 15 & x$sizeindex >=19.7 & x$sizeindex <=29, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 18 & x$sizeindex >=23 & x$sizeindex <=34, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 20 & x$sizeindex >=24 & x$sizeindex <=37, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 25 & x$sizeindex >=31 & x$sizeindex <=45, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 30 & x$sizeindex >=36 & x$sizeindex <=53, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 35 & x$sizeindex >=41 & x$sizeindex <=61, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 45 & x$sizeindex >=51 & x$sizeindex <=75, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 50 & x$sizeindex >=56 & x$sizeindex <=82, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 65 & x$sizeindex >=70 & x$sizeindex <=113, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 75 & x$sizeindex >=79 & x$sizeindex <=117, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 100 & x$sizeindex >=102 & x$sizeindex <=150, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 150 & x$sizeindex >=144 & x$sizeindex <=212, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 200 & x$sizeindex >=185 & x$sizeindex <=272, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 400 & x$sizeindex >=330 & x$sizeindex <=494, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 500 & x$sizeindex >=407 & x$sizeindex <=599, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 600 & x$sizeindex >=476 & x$sizeindex <=700, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 750 & x$sizeindex >=577 & x$sizeindex <=849, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 800 & x$sizeindex >=610 & x$sizeindex <=898, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 1000 & x$sizeindex >=739 & x$sizeindex <=1087, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 1200 & x$sizeindex >=865 & x$sizeindex <=1272, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 1500 & x$sizeindex >=1048 & x$sizeindex <=1542, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 2000 & x$sizeindex >=1343 & x$sizeindex <=1975, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 2500 & x$sizeindex >=1627 & x$sizeindex <=2393, "pass", x$balanced)
  return(x)
}

oz_sizeindex <- doesfit_func(oz_sizeindex)
library(plyr)
count(oz_sizeindex, var="balanced")


#size index plotting--------------------------------------------------------------------------------------------------------

library(magicaxis)
library(RColorBrewer)
library(scales)

##plotbits
halfblack <- alpha("darkblue", .25)
silab <- expression(Size~index~range~~(calliper~x~height))

###nice color palette
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=oz_sizeindex, xlab="Container volume (L)", ylab=silab,   xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, bg=col_vector[oz_sizeindex$climate_region],pch=21)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
title(main="Australia Tree Nurseries")
box()

dev.copy2pdf(file="master_scripts/oz_sizeindex.pdf")
dev.off()
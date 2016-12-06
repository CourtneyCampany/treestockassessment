# read data ---------------------------------------------------------------

source("functions_and_packages/plot_objects.R")
source("functions_and_packages/size_index_format.R")

#standard
standard <- read.csv("reports/container_assessment.csv")

#nursery data (6total)

fresh_dat <- read.csv("data/freshford_sizeindex.csv")
  #removing platanus B&B and Zelkova with missing height  
  fresh_dat2 <- fresh_dat[ fresh_dat$species != "platanus_x_acerifolia", ]
  fresh_dat3 <- fresh_dat2[ fresh_dat2$batch_id!="ZSG_20", ]

manor_dat <- read.csv("data/manor_sizeindex.csv")
heynes_dat <- read.csv("data/heynes_sizeindex.csv")
cleveland_dat <- read.csv("data/cleveland_sizeindex.csv")
aat_dat <- read.csv("data/adelaideadvanced_sizeindex.csv")
atf_dat <- read.csv("data/adelaidetreefarm_sizeindex.csv")

fresh <- melbs_format(fresh_dat3)
manor <- melbs_format(manor_dat)
heynes <- melbs_format(heynes_dat)
clev <- melbs_format(cleveland_dat)
aat <- melbs_format(aat_dat)
atf <- melbs_format(atf_dat)

#save formatted perth data
write.csv(fresh, "calculated_data/freshford_clean.csv", row.names = FALSE)
write.csv(manor, "calculated_data/manor_clean.csv", row.names = FALSE)
write.csv(heynes, "calculated_data/heynes_clean.csv", row.names = FALSE)
write.csv(clev, "calculated_data/cleveland_clean.csv", row.names = FALSE)
write.csv(aat, "calculated_data/aat_clean.csv", row.names = FALSE)
write.csv(atf, "calculated_data/atf_clean.csv", row.names = FALSE)

length(unique(heynes$species))
which(is.na(aat$sizeindex))

# perth plotting to test for bad data -------------------------------------

library(magicaxis)
library(RColorBrewer)
library(scales)

##plotbits
halfblack <- alpha("darkblue", .25)
silab <- expression(Size~index~range~~(calliper~x~height))

windows(7,7)

par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ logvol, data=clev, xlab="Container volume (L)", ylab=silab,   xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, col=halfblack,pch=1)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)

box()
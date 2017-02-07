
si_means_clim <- read.csv("master_scripts/si_means_climate.csv") 
si_clim <- read.csv("master_scripts/si_climate.csv")

standard <- read.csv("reports/container_assessment.csv")


###grouped histograms

si_clim$predsizeindex <- 0.452 + 0.861 * si_clim$volume

library(RColorBrewer)
library(scales)

rangelab <- "Actual - Specified Size Index"
crlab <- as.vector(unique(si_means_clim$climate_region))

palette(c(alpha("forestgreen", .4), alpha("goldenrod1",.4), alpha("navyblue", .4), alpha("firebrick2", .4),
          alpha("darkorchid3", .4), alpha("deepskyblue1", .4)))



# github.com/remkoduursma/plover
library(plover)

hist_bygroup(sizeindex-predsizeindex, climate_region, data=si_clim, breaks=300, 
             col=cols2, what="density", xlim=c(-200,200))

#100L example
trees100 <- subset(si_clim,volume==100)
hist_bygroup(sizeindex-predsizeindex, climate_region, dataset=trees100, breaks=25, what="density", 
             xlim=c(-200,200), ylim=c(0, 0.02), ylab="Frequency", xlab=rangelab)
box()
legend("topleft", crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")

#45L example
trees45 <- subset(si_clim,volume==45)
hist_bygroup(sizeindex-predsizeindex, climate_region, dataset=trees45, breaks=25, what="density", 
             xlim=c(-200,200), ylim=c(0, 0.025), ylab="Frequency", xlab=rangelab)
box()
legend("topleft", crlab ,pch=16,col=palette(), bty='n', inset=.01, title = "Region")




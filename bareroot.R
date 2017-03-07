
# read and format flemings bareroot ---------------------------------------
source("functions_and_packages/plot_objects.R")
source("functions_and_packages/functions.R")

bareroot <- read.csv("data/flemings_bareroot.csv")
  
  bareroot$date <- as.Date(bareroot$date, format = "%d/%m/%Y")
  bareroot$calliper <- with(bareroot, (diameter1 + diameter2)/2)
  bareroot$height_m <- bareroot$height/100
  bareroot$sizeindex <- with(bareroot, (height_m) * calliper)
  bareroot$species <- gsub("_x_", "_", bareroot$species)
  
  bareroot2 <-   species_variety_func(bareroot) 
    bareroot2$variety <- replace(bareroot2$variety,which(is.na(bareroot2$variety)),"plain")
    bareroot2$variety <- as.factor(bareroot2$variety)
    
# split pre and post planting data ----------------------------------------
pre_br <- bareroot2[bareroot2$date == "2016-06-27",]
    
bareroot3 <- bareroot2[! bareroot2$date == "2016-06-27",]
    
# preplanting summary---------------------------------------------------
library(doBy)
se <- function(x) sd(x)/sqrt(length(x))
silab <- expression(Size~index~range~~(calliper~x~height))
cols <- c("gold","gold","darkgreen","darkgreen")

#initial differences
pre_br_agg <- summaryBy(height_m+calliper+sizeindex ~ date+species+grade, data=pre_br, FUN=c(mean, se))

boxplot(sizeindex ~ variety*grade, data = droplevels(pre_br[pre_br$genus_species=="fraxinus_pennsylvanica",]),
        col=cols, ylim=c(10,45), xaxt='n', main= "Fraxinus_pennsylvanica", ylab=silab)
axis(1, at = 1:4,labels=c("Cimmaron","Urbanite", "Cimmaron","Urbanite"))
legend("topright", col=c("gold","darkgreen"), pch=15, legend=c("Advanced", "Regular"), inset=0.01, bty='n')

boxplot(height/100 ~ variety*grade, data = droplevels(pre_br[pre_br$genus_species=="fraxinus_pennsylvanica",]),
        col=cols,  xaxt='n', main= "Fraxinus_pennsylvanica", ylab="height (m)")
axis(1, at = 1:4,labels=c("Cimmaron","Urbanite", "Cimmaron","Urbanite"))
legend("topright", col=c("gold","darkgreen"), pch=15, legend=c("Advanced", "Regular"), inset=0.01, bty='n')

boxplot(calliper ~ variety*grade, data = droplevels(pre_br[pre_br$genus_species=="fraxinus_pennsylvanica",]),
        col=cols,  xaxt='n', main= "Fraxinus_pennsylvanica", ylab="Calliper at 30 cm (mm)")
axis(1, at = 1:4,labels=c("Cimmaron","Urbanite", "Cimmaron","Urbanite"))
legend("topright", col=c("gold","darkgreen"), pch=15, legend=c("Advanced", "Regular"), inset=0.01, bty='n')


# growth through time plotting --------------------------------------------
library(wesanderson)
bareroot3$unique_id <- with(bareroot3, paste(species, grade, sep="-"))

bareroot_agg <- summaryBy(height_m+calliper+sizeindex ~ date+grade+unique_id+genus_species+variety+species,
                          data=bareroot3, FUN=c(mean, se))

library(ggsci)
mypal <- pal_lancet("lanonc", alpha = 0.9)(9)
palette(mypal)

plot(sizeindex.mean~date, data=bareroot_agg, ylab=silab, xlab="", type='n', ylim=c(0,90))

ids <- unique(bareroot_agg$unique_id)
for(id in ids){
  points(sizeindex.mean ~ date, data=bareroot_agg[bareroot_agg$grade=="advanced",], 
         subset=unique_id==id, type='l', col=as.factor(genus_species),
         lty=c(1,2), lwd=2)
  }

plot(sizeindex.mean~date, data=bareroot_agg, ylab=silab, xlab="", type='n', ylim=c(0,90))

ids <- unique(bareroot_agg$unique_id)
for(id in ids){
  points(sizeindex.mean ~ date, data=bareroot_agg[bareroot_agg$grade=="regular",], 
         subset=unique_id==id, type='l', col=as.factor(genus_species),
         lty=rep(c(1,2), times=6), lwd=2)
}


library(ggplot2)
ggplot(data = bareroot_agg, 
       aes(x=date, y=sizeindex.mean)) + geom_line(aes(colour=species,linetype=grade), size=1) +
    theme_bw() +
    scale_color_d3("category20")


# stats -------------------------------------------------------------------
library(visreg)
library(multcomp)
library(nlme)

bareroot_last <- bareroot2[bareroot2$date == "2016-12-02",]


# si_mod <- lme(sizeindex ~ species+grade + (1|tree), data=bareroot_last)
si_mod <- lme(sizeindex ~ genus_species+grade,  random=~1|tree, data=bareroot_last)
#####model parameters for plotting
summary(si_mod)
visreg(si_mod)
anova(si_mod)

diameter_mod <- lme(calliper ~ genus_species+grade, random=~1|tree, data=bareroot_last)
#####model parameters for plotting
summary(diameter_mod)
coef(diameter_mod)
anova(diameter_mod)

height_mod <- lme(height_m ~ species+grade, random=~1|tree, data=bareroot_last)
#####model parameters for plotting
summary(height_mod)
coef(height_mod)
anova(height_mod)



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

# summary of treatments ---------------------------------------------------
library(doBy)
se <- function(x) sd(x)/sqrt(length(x))
silab <- expression(Size~index~range~~(calliper~x~height))

br_agg <- summaryBy(height_m+calliper+sizeindex ~ date+species+grade, data=bareroot, FUN=c(mean, se))


plot(sizeindex.mean ~ date, data = br_agg, col=species, xlab="", ylab = silab, pch=c(16,17)[grade],
     ylim=c(0,100))


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

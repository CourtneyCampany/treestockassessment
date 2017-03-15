source("functions_and_packages/functions.R")

# read and format flemings bareroot ---------------------------------------
bareroot <- read.csv("data/flemings_bareroot.csv")
  
  bareroot$date <- as.Date(bareroot$date, format = "%d/%m/%Y")
  bareroot$calliper <- with(bareroot, (diameter1 + diameter2)/2)
  bareroot$height_m <- bareroot$height/100
  bareroot$sizeindex <- with(bareroot, (height_m) * calliper)
  bareroot$species <- gsub("_x_", "_", bareroot$species)
  
  bareroot2 <-   species_variety_func(bareroot) 
    bareroot2$variety <- replace(bareroot2$variety,which(is.na(bareroot2$variety)),"plain")
    bareroot2$variety <- as.factor(bareroot2$variety)
    
write.csv(bareroot2, "bareroot/bareroot_clean.csv", row.names = FALSE)    
    
# split pre and post planting data ----------------------------------------
pre_br <- bareroot2[bareroot2$date == "2016-06-27",]
    
bareroot3 <- bareroot2[! bareroot2$date == "2016-06-27",]
    
# preplanting summary---------------------------------------------------

pre_br_agg <- doBy::summaryBy(height_m+calliper+sizeindex ~ date+species+grade, data=pre_br, FUN=c(mean, se))

silab <- expression(Size~index~range~~(calliper~x~height))
gradecols <- c("deepskyblue2", "firebrick3")
speclab <- c("Acer rubrum bowhall","Acer rubrum octoberglory","Fraxinus pennsylvanica \ncimmaron",
             "Fraxinus pennsylvanica \nurbanite","Platanus acerifolia","Platanus acerifolia liberty",
             "Pyrus calleryana \ncapital","Pyrus calleryana \nchanticleer")
pre_br_sort <- arrange(pre_br, species)
pre_br_sort$id <- with(pre_br_sort, paste(species, grade, sep="-"))

# bar(sizeindex, factors=c(grade, species), dataframe=pre_br_sort, errbar=TRUE, ylim=c(0,70), xlab="", xaxt='n',
#     mar=c(9,5,2,2), ylab=silab, col=gradecols)
# axis(1, at=c(2,5,8,11,14,17,20,23), labels=FALSE)
# text(c(3,6,9,12,15,18,21,24), par("usr")[3]-7, srt=45,labels=speclab,xpd=TRUE, pos=2)


# boxplots of SI, height and diameter at start ----------------------------

windows()
#SI
par(mar=c(9,5,2,2))
boxplot(sizeindex ~ id, data = pre_br_sort,col=gradecols, xaxt='n',  ylab=silab,
        at=c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17, 19, 20, 22,23), outline=FALSE)
axis(1, at=c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5, 22.5), labels=FALSE)
legend("topright", c("Advanced", "Regular"), pch=22, pt.bg=gradecols, inset=.01, bty='n', pt.cex=1.5)
text(c(2,5,8,11,14,17,20,23), par("usr")[3]-4,  srt=45,labels=speclab,xpd=TRUE, pos=2)

#Calliper
windows()
par(mar=c(9,5,2,2))
boxplot(calliper ~ id, data = pre_br_sort,col=gradecols, xaxt='n',  ylab="Calliper @ 30cm (mm)",
        at=c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17, 19, 20, 22,23), outline=FALSE)
axis(1, at=c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5, 22.5), labels=FALSE)
legend("topright", c("Advanced", "Regular"), pch=22, pt.bg=gradecols, inset=.01, bty='n', pt.cex=1.5)
text(c(2,5,8,11,14,17,20,23), par("usr")[3]-1,  srt=45,labels=speclab,xpd=TRUE, pos=2)

#Height
windows()
par(mar=c(9,5,2,2))
boxplot(height_m ~ id, data = pre_br_sort,col=gradecols, xaxt='n',  ylab="Height (m)",
        at=c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17, 19, 20, 22,23), outline=FALSE)
axis(1, at=c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5, 22.5), labels=FALSE)
legend("topright", c("Advanced", "Regular"), pch=22, pt.bg=gradecols, inset=.01, bty='n', pt.cex=1.5)
text(c(2,5,8,11,14,17,20,23), par("usr")[3]-.1,  srt=45,labels=speclab,xpd=TRUE, pos=2)


# growth through time plotting ----------------------------------------------------------------------------

bareroot3$unique_id <- with(bareroot3, paste(species, grade, sep="-"))

bareroot_agg <- summaryBy(height_m+calliper+sizeindex ~ date+grade+unique_id+genus_species+variety+species,
                          data=bareroot3, FUN=c(mean, se))
  bareroot_agg$species <- as.factor(bareroot_agg$species)
  
speclab2 <- c("Acer rubrum bowhall","Acer rubrum octoberglory","Fraxinus pennsylvanica cimmaron",
                "Fraxinus pennsylvanica urbanite","Platanus acerifolia","Platanus acerifolia liberty",
                "Pyrus calleryana capital","Pyrus calleryana chanticleer")  

# library(ggsci)
# mypal <- pal_lancet("lanonc", alpha = 0.9)(8)
library(RColorBrewer)
mypal <- brewer.pal(8, "Dark2")
palette(mypal)


reg <- bareroot_agg[bareroot_agg$grade=="regular",]
adv <- bareroot_agg[bareroot_agg$grade=="advanced",]
xAT <- unique(reg$date)

# Size Index --------------------------------------------------------------

windows()
par(las=1,mgp=c(3.5,1,0),mfrow=c(2,1), mar=c(0,5,1,1))   
#plot regular trees
plot(sizeindex.mean~date, data=reg, ylab="", xaxt='n',xlab="", type='n', ylim=c(0,90))
axis.Date(1, at=xAT, labels=FALSE)

ids <- unique(reg$unique_id)
for(id in ids){
  points(sizeindex.mean ~ date, data=reg, lwd=2, 
         subset=unique_id==id, type='o', col=mypal[as.factor(species)])

with(reg[reg$unique_id==id,], arrows(date, sizeindex.mean, date, sizeindex.mean+sizeindex.se, 
                                     angle=90,length=0.03, cex=2,col=mypal[species]))

with(reg[reg$unique_id==id,], arrows(date, sizeindex.mean, date, sizeindex.mean-sizeindex.se, 
                                     angle=90,length=0.03, cex=2,col=mypal[species]))
}
legend("bottomright", expression(bold("Regular")), inset=.01, bty='n')
mtext(silab,2,line=-2.5, outer = TRUE, at=.5, las=0)
legend("topleft", legend = speclab2, lty=1,lwd=2, col=palette(),bty='n', cex=.8)
#advanced
par(mar=c(5,5,0,1))
plot(sizeindex.mean~date, data=adv, ylab="", xaxt='n',xlab="", type='n', ylim=c(0,90))
axis.Date(1, at=xAT)

ids <- unique(adv$unique_id)
for(id in ids){
  points(sizeindex.mean ~ date, data=adv, lwd=2, 
         subset=unique_id==id, type='o', col=mypal[as.factor(species)])
  
  with(adv[adv$unique_id==id,], arrows(date, sizeindex.mean, date, sizeindex.mean+sizeindex.se, 
                                       angle=90,length=0.03, cex=2,col=mypal[species]))
  
  with(adv[adv$unique_id==id,], arrows(date, sizeindex.mean, date, sizeindex.mean-sizeindex.se, 
                                       angle=90,length=0.03, cex=2,col=mypal[species]))
}
legend("bottomright", expression(bold("Advanced")), inset=.01, bty='n')



# library(ggplot2)
# ggplot(data = bareroot_agg,
#        aes(x=date, y=sizeindex.mean)) + 
#         geom_line(aes(colour=species,linetype=grade), size=1) +
#        geom_point() +
#        geom_errorbar(aes(ymin=sizeindex.mean -sizeindex.se, ymax =sizeindex.mean + sizeindex.se ), width=.1) + 
#        theme_bw() +
#        scale_color_d3("category20")



# Height ------------------------------------------------------------------
windows()
par(las=1,mgp=c(3.5,1,0),mfrow=c(2,1), mar=c(0,5,1,1))   
#plot regular trees
plot(height_m.mean~date, data=reg, ylab="", xaxt='n',xlab="", type='n', ylim=c(0.5,3))
axis.Date(1, at=xAT, labels=FALSE)

ids <- unique(reg$unique_id)
for(id in ids){
  points(height_m.mean ~ date, data=reg, lwd=2, 
         subset=unique_id==id, type='o', col=mypal[as.factor(species)])
  
  with(reg[reg$unique_id==id,], arrows(date, height_m.mean, date, height_m.mean+height_m.se, 
                                       angle=90,length=0.03, cex=2,col=mypal[species]))
  
  with(reg[reg$unique_id==id,], arrows(date, height_m.mean, date, height_m.mean-height_m.se, 
                                       angle=90,length=0.03, cex=2,col=mypal[species]))
}
legend("bottomright", expression(bold("Regular")), inset=.01, bty='n')
mtext("Height (m)",2,line=-2.5, outer = TRUE, at=.5, las=0)
legend("topleft", legend = speclab2, lty=1,lwd=2, col=palette(),bty='n', cex=.8)
#advanced
par(mar=c(5,5,0,1))
plot(height_m.mean~date, data=adv, ylab="", xaxt='n',xlab="", type='n', ylim=c(0.5,3))
axis.Date(1, at=xAT)

ids <- unique(adv$unique_id)
for(id in ids){
  points(height_m.mean ~ date, data=adv, lwd=2, 
         subset=unique_id==id, type='o', col=mypal[as.factor(species)])
  
  with(adv[adv$unique_id==id,], arrows(date, height_m.mean, date, height_m.mean+height_m.se, 
                                       angle=90,length=0.03, cex=2,col=mypal[species]))
  
  with(adv[adv$unique_id==id,], arrows(date, height_m.mean, date, height_m.mean-height_m.se, 
                                       angle=90,length=0.03, cex=2,col=mypal[species]))
}
legend("bottomright", expression(bold("Advanced")), inset=.01, bty='n')

# Diameter ----------------------------------------------------------------

windows()
par(las=1,mgp=c(3.5,1,0),mfrow=c(2,1), mar=c(0,5,1,1))   
#plot regular trees
plot(calliper.mean~date, data=reg, ylab="", xaxt='n',xlab="", type='n', ylim=c(10,35))
axis.Date(1, at=xAT, labels=FALSE)

ids <- unique(reg$unique_id)
for(id in ids){
  points(calliper.mean ~ date, data=reg, lwd=2, 
         subset=unique_id==id, type='o', col=mypal[as.factor(species)])
  
  with(reg[reg$unique_id==id,], arrows(date, calliper.mean, date, calliper.mean+calliper.se, 
                                       angle=90,length=0.03, cex=2,col=mypal[species]))
  
  with(reg[reg$unique_id==id,], arrows(date, calliper.mean, date, calliper.mean-calliper.se, 
                                       angle=90,length=0.03, cex=2,col=mypal[species]))
}
legend("bottomright", expression(bold("Regular")), inset=.01, bty='n')
mtext("Calliper at 30cm (mm)",2,line=-2.5, outer = TRUE, at=.5, las=0)
legend("topleft", legend = speclab2, lty=1,lwd=2, col=palette(),bty='n', cex=.8)
#advanced
par(mar=c(5,5,0,1))
plot(calliper.mean~date, data=adv, ylab="", xaxt='n',xlab="", type='n', ylim=c(10,35))
axis.Date(1, at=xAT)

ids <- unique(adv$unique_id)
for(id in ids){
  points(calliper.mean ~ date, data=adv, lwd=2, 
         subset=unique_id==id, type='o', col=mypal[as.factor(species)])
  
  with(adv[adv$unique_id==id,], arrows(date, calliper.mean, date, calliper.mean+calliper.se, 
                                       angle=90,length=0.03, cex=2,col=mypal[species]))
  
  with(adv[adv$unique_id==id,], arrows(date, calliper.mean, date, calliper.mean-calliper.se, 
                                       angle=90,length=0.03, cex=2,col=mypal[species]))
}
legend("bottomright", expression(bold("Advanced")), inset=.01, bty='n')

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


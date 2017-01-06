source("functions/functions.R")

##Shape parameters for alpine nursery

shape <- read.csv("data/alpine_shape.csv")
shape$crownlength <- with(shape, canopy_top - canopy_bottom)

branch <- read.csv("data/alpine_branch.csv")
alpine_species <- read.csv("data/alpine_species.csv", stringsAsFactors = FALSE)


##function to calculate shape parameters
treeshape_func <- function(x){
  x$branchgroup <- ifelse(x$branchper30 <= 4, "low",
                      ifelse(x$branchper30 >=5 & x$branchper30 <= 10, "medium",
                        "high"))
  print("successfully grouped branchiness")
  
  x$crownlength <- with(x, canopy_top - canopy_bottom)
  # if(x$crownlength <= 0) stop("there is bad data because crownlength must be positive")
  print("successfully calculated crown size")
  
  x$crownvolume <- with(x, (pi* (maxbranchradius)^2 * (crownlength/3))/1000000) #units in cm
  print("crown area calculated successfully")
  
return(x)
}

alpine_shape <- treeshape_func(shape)

alpine_shape_agg <- doBy::summaryBy(maxbranchradius+branchper30+crownlength+canopy_top+crownvolume ~ volume+species, 
                                    data=alpine_shape, FUN=c(mean), keep.names = TRUE)


###add species information
alpine_trees <- merge(alpine_shape_agg, alpine_species, by="species", all=TRUE)

alpine_shape$crownratio <- with(alpine_shape, crownlength/canopy_top)
alpine_shape$crownratio2 <- with(alpine_shape, (maxbranchradius*2)/crownlength)
alpine_shape$crownratio4 <- with(alpine_shape, crownvolume/crownlength)

write.csv(alpine_trees, "calculated_data/alpine_trees.csv", row.names = FALSE)



##preliminary plots

plot(crownlength.mean ~ maxbranchradius.mean, data=alpine_shape_agg, col=as.factor(species), pch=16, ylim=c(0,600), xlim=c(0,200))
plot(crownlength.mean ~ branchper30.mean, data=alpine_shape_agg, col=as.factor(species), pch=16, ylim=c(0,600), xlim=c(0,20))
plot(canopy_top.mean ~ branchper30.mean, data=alpine_shape_agg, col=as.factor(species), pch=16, ylim=c(0,800), xlim=c(0,20))
plot(canopy_top.mean ~ crownlength.mean, data=alpine_shape_agg, col=as.factor(species), pch=16, ylim=c(0,700), xlim=c(0, 600))
plot(canopy_top.mean ~ maxbranchradius.mean, data=alpine_shape_agg, col=as.factor(species), pch=16, ylim=c(0,700), xlim=c(0, 200))
plot(crownvolume.mean ~ maxbranchradius.mean, data=alpine_shape_agg, col=as.factor(species), pch=16)

plot(crownvolume ~ volume, data=alpine_shape_agg, col=as.factor(species), pch=16)
plot(branchper30.mean ~ volume, data=alpine_shape_agg, col=as.factor(species), pch=16)
plot(maxbranchradius.mean ~ volume, data=alpine_shape_agg, col=as.factor(species), pch=16)

plot(lengthtobranchiness ~ volume, data=alpine_trees, col=as.factor(species), pch=16)

par(mar=c(9,5,1,1))
boxplot(branchper30 ~ species, data=alpine_shape, las=2)
boxplot(crownratio ~ species, data=alpine_shape, las=2)
boxplot(crownratio2 ~ species, data=alpine_shape, las=2)
boxplot(crownvolume ~ volume, data=alpine_shape, las=2)


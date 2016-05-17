source("functions/functions.R")

##Shape parameters for alpine nursery

shape <- read.csv("data/alpine_shape.csv")
branch <- read.csv("data/alpine_branch.csv")
alpine_species <- read.csv("data/alpine_species.csv", stringsAsFactors = FALSE)


##function to calculate shape parameters
treeshape_func <- function(x){
  x$branchgroup <- ifelse(x$branchper30 <= 4, "low",
                      ifelse(x$branchper30 >=5 & x$branchper30 <= 10, "medium",
                        "high"))
  print("successfully grouped branchiness")
  
  x$crownlength <- with(x, canopy_top - canopy_bottom)
  print("successfully calculated crown size")
  x$crowncone <- with(x, (pi*maxbranchradius)*(maxbranchradius+sqrt((crownlength)^2+(maxbranchradius)^2))) #cm for lengths
  print("crown area calculated successfully")
return(x)
}

alpine_shape <- treeshape_func(shape)

alpine_shape_agg <- doBy::summaryBy(maxbranchradius+branchper30+crownlength+canopy_top+crowncone ~ volume+species, 
                                    data=alpine_shape, FUN=c(mean,se))


###add species information
test <- merge(alpine_shape, alpine_species, by="species", all=TRUE)





##preliminary plots

plot(crownlength.mean ~ maxbranchradius.mean, data=alpine_shape_agg, col=as.factor(species), pch=16, ylim=c(0,600), xlim=c(0,200))
plot(crownlength.mean ~ branchper30.mean, data=alpine_shape_agg, col=as.factor(species), pch=16, ylim=c(0,600), xlim=c(0,20))
plot(canopy_top.mean ~ branchper30.mean, data=alpine_shape_agg, col=as.factor(species), pch=16, ylim=c(0,800), xlim=c(0,20))
plot(canopy_top.mean ~ crownlength.mean, data=alpine_shape_agg, col=as.factor(species), pch=16, ylim=c(0,700), xlim=c(0, 600))
plot(canopy_top.mean ~ maxbranchradius.mean, data=alpine_shape_agg, col=as.factor(species), pch=16, ylim=c(0,700), xlim=c(0, 200))
plot(crowncone.mean ~ maxbranchradius.mean, data=alpine_shape_agg, col=as.factor(species), pch=16)

plot(crowncone.mean ~ volume, data=alpine_shape_agg, col=as.factor(species), pch=16)
plot(branchper30.mean ~ volume, data=alpine_shape_agg, col=as.factor(species), pch=16)
plot(maxbranchradius.mean ~ volume, data=alpine_shape_agg, col=as.factor(species), pch=16)

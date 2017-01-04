
# read nursery clean data -------------------------------------------------

source("functions_and_packages/plot_objects.R")
source("functions_and_packages/size_index_format.R")

##standard
standard <- read.csv("reports/container_assessment.csv")

  ##NSW
  alpine<- read.csv("calculated_data/alpine_clean.csv")
    alpine$batch_id <- as.factor(alpine$batch_id)
  treesimpact <- read.csv("calculated_data/treesimpact_clean.csv")
    treesimpact$batch_id <- as.factor(treesimpact$batch_id)
  kemps<- read.csv("calculated_data/kemps_clean.csv")
  mangrove<- read.csv("calculated_data/mangrove_clean.csv")
  
  ##NT
  darwin<- read.csv("calculated_data/darwin_clean.csv")
  
  ##SA
  freshford<- read.csv("calculated_data/freshford_clean.csv")
  manor<- read.csv("calculated_data/manor_clean.csv")
  adelaideadvanced<- read.csv("calculated_data/aat_clean.csv")
  adelaidetreefarm<- read.csv("calculated_data/atf_clean.csv")
  heynes<- read.csv("calculated_data/heynes_clean.csv")
  cleveland<- read.csv("calculated_data/cleveland_clean.csv")

  ##VIC
  ett<- read.csv("calculated_data/ett_clean.csv")
  fleming<- read.csv("calculated_data/fleming_clean.csv")
  mtwilly<- read.csv("calculated_data/mtwilly_clean.csv")
  speciality<- read.csv("calculated_data/speciality_clean.csv")
  
  ##WA
  ellenby<- read.csv("calculated_data/ellenby_clean.csv")
  arborwest<- read.csv("calculated_data/arborwest_clean.csv")
  benara<- read.csv("calculated_data/benara_clean.csv")

# merge data to a master file ---------------------------------------------
oz_sizeindex<- Reduce(function(...) merge(..., all=TRUE), 
               list(darwin, ett, fleming,mtwilly,speciality,ellenby,benara,arborwest, freshford,manor, heynes,
                    cleveland, adelaidetreefarm, adelaideadvanced, treesimpact , 
                    alpine, kemps[, !names(kemps) %in% "site"],mangrove[, !names(mangrove) %in% "site"]))
               

# variable formatting -----------------------------------------------------

  ##add climate zone
  oz_sizeindex <- add_campaign_region(oz_sizeindex)
  
  ##simplify hybrids (remove genus_x_hybrid)
  oz_sizeindex$species <- gsub("_x_", "_", oz_sizeindex$species)
  
  ##drop batch_id2
  oz_sizeindex <- oz_sizeindex[,  !names(oz_sizeindex) %in% "batch_id2"]
  
# genus-species-variety function ------------------------------------------

species_variety_func <- function(x){  
  
  dat <- x$species
  
  splitnames <- strsplit(dat, "_")
  #new column with only variety
  variety <- lapply(splitnames, FUN=function(y){y[3]})
    variety_dat <- data.frame(matrix(unlist(variety), ncol=1, byrow=TRUE))
    names(variety_dat)[1] <- "variety"
  print("variety made successfully")  
  #new column with only genus species  
  genus_species <- lapply(splitnames, FUN=function(z){z[1:2]})
    genus_species_dat <- data.frame(matrix(unlist(genus_species), ncol=2, byrow=TRUE))
      names(genus_species_dat)[1:2] <- c("genus", "species")
      genus_species_dat$genus_species <- paste(genus_species_dat$genus, genus_species_dat$species, sep="_")
  print("genus-species made successfully")
  
  speciescolumns <- cbind(genus_species_dat[3],variety_dat)    
  alldat <- cbind(x, speciescolumns)
  alldat$variety <- as.character(alldat$variety)
  print("merge with orginal dfr worked")
  
 return(alldat)
   
}
  
oz_sizeindex2 <-   species_variety_func(oz_sizeindex)  

length(unique(oz_sizeindex2$genus_species))
length(unique(oz_sizeindex2$volume))
length(unique(oz_sizeindex2$species))
range(oz_sizeindex2$volume) 


# merge species origin ----------------------------------------------------

origin <- read.csv("data/species_origin.csv")
oz_sizeindex3 <- merge(oz_sizeindex2, origin, all=TRUE)


# reformat nursery names --------------------------------------------------

nurseryname_format <- function(x) {
  x$nursery <- gsub("alp", "alpine", x$nursery)
  x$nursery <- gsub("a_mm", "mangrove mountain", x$nursery)
  x$nursery <- gsub("treesimpact", "trees impact", x$nursery)
  x$nursery <- gsub("a_kc", "andreasens", x$nursery)
  x$nursery <- gsub("flem", "flemings", x$nursery)
  x$nursery <- gsub("spec", "speciality", x$nursery)
  x$nursery <- gsub("mtwil", "mt william", x$nursery)
  x$nursery <- gsub("dph", "darwin plant wholesalers", x$nursery)
  x$nursery <- gsub("ett", "established tree transplanters", x$nursery)
  x$nursery <- gsub("adelaideadvanced", "adelaide advanced", x$nursery)
  x$nursery <- gsub("adelaidetreefarm", "adelaide tree farm", x$nursery)
  x$nursery <- gsub("ellenby", "ellenby tree farm  tree farm", x$nursery)
  return(x)
} 

oz_sizeindex4 <- nurseryname_format(oz_sizeindex3)


#save masterfile of sizeindex data
write.csv(oz_sizeindex4, "calculated_data/oz_sizeindex.csv", row.names = FALSE)


# Does Fit Size Index? ---------------------------------------------------------

######I need to add new volumes to this function likely......(post adelaide)
oz_sizeindex <- doesfit_func(oz_sizeindex)
library(plyr)
count(oz_sizeindex, var="balanced")


#size index plotting--------------------------------------------------------
oz_si_agg <- summaryBy(logSI ~ species + nursery+volume, 
                       data=oz_sizeindex2, FUN=mean, keep.names = TRUE)

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

# windows(7,7)
png(filename = "master_scripts/ozsizeindex_batch_means.png", width = 5, height = 5, units = "in", res= 600)
par(mar=c(5,5,2,1),cex.axis=1, cex.lab=1.25,las=0,mgp=c(3,1,0))
plot(logSI ~ log10(volume), data=oz_si_agg, xlab="Container volume (L)", ylab=silab,   xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     axes=FALSE, cex=1.25, bg="olivedrab3",pch=21)

magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment
lines(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2.5)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2.5)

legend("topleft", "AS2303 Size Index Range" ,lty=1, lwd=2,bty='n', inset=.01)
#legend("bottomright", c("New South Wales", "Northern Territory", "Perth", "Victoria"), pch=21, pt.bg =cols2, inset=.025)
title(main="Australia Tree Nurseries")
box()

# dev.copy2pdf(file="master_scripts/oz_sizeindex_batch_means.pdf")
dev.off()

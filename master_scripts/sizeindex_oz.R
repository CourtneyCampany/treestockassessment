
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
  
  ##QLD
  logans <- read.csv("calculated_data/logans_clean.csv")
  pallara <- read.csv("calculated_data/pallara_clean.csv")
  greenstock <- read.csv("calculated_data/greenstock_clean.csv")
  ibrox <- read.csv("calculated_data/ibrox_clean.csv")
  plantsdirect <- read.csv("calculated_data/plantsdirect_clean.csv")

# merge data to a master file ---------------------------------------------
oz_sizeindex<- Reduce(function(...) merge(..., all=TRUE), 
               list(darwin, ett, fleming,mtwilly,speciality,ellenby,benara,arborwest, freshford,manor, heynes,
                    cleveland, adelaidetreefarm, adelaideadvanced, treesimpact , 
                    alpine, kemps[, !names(kemps) %in% "site"],mangrove[, !names(mangrove) %in% "site"],
                    logans, pallara, greenstock, ibrox, plantsdirect))
               

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
  x$nursery <- gsub("ellenby", "ellenby tree farm", x$nursery)
  x$nursery <- gsub("plantsdirect", "plants direct", x$nursery)
  return(x)
} 

oz_sizeindex4 <- nurseryname_format(oz_sizeindex3)

##calculate slenderness index
oz_sizeindex4$slenderness <- with(oz_sizeindex4, height_m/calliper300)

#save masterfile of sizeindex data
write.csv(oz_sizeindex4, "calculated_data/oz_sizeindex.csv", row.names = FALSE)


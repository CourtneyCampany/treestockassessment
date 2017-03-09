source("functions_and_packages/size_index_format.R")


##this script extracts the leaf data from the tree shape datafiles


# read in tree shape data -------------------------------------------------
leaf_files <- list.files(path = "data//", pattern="*shape.csv", full.names = TRUE)
leaf_list <- lapply(leaf_files, function(x) read.csv(x))

file_names <- gsub("data//", "", leaf_files)
file_names <- gsub("_shape.csv", "", file_names)

##add nursery names
for (i in seq_along(leaf_list)){
  leaf_list[[i]]$nursery <- file_names[i]
}  


# collect leaf data -------------------------------------------------------

leaf_get_func <- function(x){
  
  #split dataframe into shape and leaves
  y <- x[, c("nursery", "batch_id", "species", "volume", "treenumb","leafarea1", "leafarea2", "leafarea3", 
                "leafmass1", "leafmass2", "leafmass3")]
  return(y)
}

nursery_leaves <- lapply(leaf_list, function(x) leaf_get_func(x))

nursery_leaves2 <- plyr::rbind.fill(nursery_leaves)
nursery_leaves3 <- nursery_leaves2[complete.cases(nursery_leaves2),]

lma_func <- function(x){
  x$sla1 <- with(x, leafarea1/leafmass1)
  x$sla2 <- with(x, leafarea2/leafmass2)
  x$sla3 <- with(x, leafarea3/leafmass3)
  
  x$sla_tree <- with(x, (sla1+sla2+sla3)/3)
  x$mass_tree <- with(x, (leafmass1+leafmass2+leafmass3)/3)
  x$area_tree <- with(x, (leafarea1+leafarea2+leafarea3)/3)
  
  y <- doBy::summaryBy(sla_tree + mass_tree + area_tree ~ nursery + species + volume, 
                       data=x, FUN=mean, keep.names = TRUE)
  
  return(y)
}

nursery_leaves4 <- lma_func(nursery_leaves3)

# variable formatting -----------------------------------------------------

#clean nursery names
nurseryname_format <- function(x) {
  x$nursery <- gsub("mangrovemtn", "mangrove mountain", x$nursery)
  x$nursery <- gsub("treesimpact", "trees impact", x$nursery)
  x$nursery <- gsub("mtwill", "mt william", x$nursery)
  x$nursery <- gsub("fleming", "flemings", x$nursery)
  x$nursery <- gsub("dph", "darwin plant wholesalers", x$nursery)
  x$nursery <- gsub("ett", "established tree transplanters", x$nursery)
  x$nursery <- gsub("adelaideadvanced", "adelaide advanced", x$nursery)
  x$nursery <- gsub("adelaidetreefarm", "adelaide tree farm", x$nursery)
  x$nursery <- gsub("ellenby", "ellenby tree farm", x$nursery)
  x$nursery <- gsub("plantsdirect", "plants direct", x$nursery)
  return(x)
} 

nursery_leaves5 <- nurseryname_format(nursery_leaves4)

#--add climate zone by nusery name
add_campaign_region <- function(x){
  
  x$climate_region <-ifelse(x$nursery == "darwin plant wholesalers", "Northern Territory", "imlost")
  x$climate_region <-ifelse(x$nursery == "alpine" | x$nursery == "andreasens" | x$nursery == "mangrove mountain" 
                            |x$nursery=="trees impact", "New South Wales", x$climate_region)
  x$climate_region <-ifelse(x$nursery == "flemings" | x$nursery == "speciality" | 
                              x$nursery == "established tree transplanters" | 
                              x$nursery == "mt william", "Victoria", x$climate_region )
  x$climate_region <-ifelse(x$nursery == "ellenby tree farm"|x$nursery == "benara"|x$nursery =="arborwest", 
                            "Western Australia", x$climate_region)
  x$climate_region <-ifelse(x$nursery == "heynes"|x$nursery == "cleveland"|x$nursery =="manor"|
                              x$nursery == "freshford"|x$nursery == "adelaide advanced"|
                              x$nursery == "adelaide tree farm",
                            "South Australia", x$climate_region)
  x$climate_region <-ifelse(x$nursery == "logans"|x$nursery == "ibrox"|x$nursery =="pallara"|
                              x$nursery == "greenstock"|x$nursery == "plants direct",
                            "Queensland", x$climate_region)
  x$climate_region <- as.factor(x$climate_region)
  
  return(x)
}

##add climate zone
nursery_leaves6 <- add_campaign_region(nursery_leaves5)


##simplify hybrids (remove genus_x_hybrid)
nursery_leaves6$species <- gsub("_x_", "_", nursery_leaves6$species)

##genus species function
nursery_leaves7 <-   species_variety_func(nursery_leaves6) 


# merge species origin ----------------------------------------------------

origin <- read.csv("data/species_origin.csv")
nursery_leaves8 <- merge(nursery_leaves7, origin)


evercol <- scales::alpha("forestgreen", .3)
decidcol <- scales::alpha("goldenrod1", .3)

plot(sla_tree~volume, data=nursery_leaves8, col=c(decidcol,evercol)[leaf_type], pch=16)
plot(1/sla_tree~volume, data=nursery_leaves8, col=c(decidcol,evercol)[leaf_type], pch=16)

plot(sla_tree~volume, data=nursery_leaves8[nursery_leaves8$volume < 1000,],
     col=c(decidcol,evercol)[leaf_type], pch=16)


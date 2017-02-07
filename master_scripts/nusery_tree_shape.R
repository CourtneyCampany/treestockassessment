source("functions_and_packages/size_index_format.R")

# read in tree shape data -------------------------------------------------
shape_files <- list.files(path = "data//", pattern="*shape.csv", full.names = TRUE)
shape_list <- lapply(shape_files, function(x) read.csv(x))

  shape_vars <- gsub("data//", "", shape_files)
  shape_vars <- gsub("_shape.csv", "", shape_vars)

##add nursery names
for (i in seq_along(shape_list)){
  shape_list[[i]]$nursery <- shape_vars[i]
}  


##function to calculate shape parameters------------------------------------------------------------
treeshape_func <- function(x){

  x$crown_length <- with(x, canopy_top - canopy_bottom)
  # if(x$crownlength <= 0) stop("there is bad data because crownlength must be positive")
  print("successfully calculated crown size")
  
  x$crown_volume <- with(x, (pi* (maxbranchradius)^2 * (crown_length/3))/1000000) #units in cm
  print("crown area calculated successfully")
  
  x$crown_ratio <- with(x, crown_volume/crown_length)
  print("crown ratio calculated successfully")
  
  x$crown_spread <- with(x, maxbranchradius * 2)
  print("crown spread calculated successfully")
  
  #split dataframe into shape and leaves
  y <- x[, c("nursery", "batch_id", "species", "volume", "treenumb", "crown_length", "branchper30", 
             "crown_spread","crown_ratio" )]
  
  return(y)
  
}

nursery_shape <- lapply(shape_list, function(x) treeshape_func(x))


# unlist shape data -------------------------------------------------------
library(plyr)
nursery_shape2 <- rbind.fill(nursery_shape)


# variable formatting -----------------------------------------------------

#clean nursery names
nurseryname_format <- function(x) {
  x$nursery <- gsub("mangrovemtn", "mangrove mountain", x$nursery)
  x$nursery <- gsub("treesimpact", "trees impact", x$nursery)
  x$nursery <- gsub("mtwill", "mt william", x$nursery)
  x$nursery <- gsub("dph", "darwin plant wholesalers", x$nursery)
  x$nursery <- gsub("ett", "established tree transplanters", x$nursery)
  x$nursery <- gsub("adelaideadvanced", "adelaide advanced", x$nursery)
  x$nursery <- gsub("adelaidetreefarm", "adelaide tree farm", x$nursery)
  x$nursery <- gsub("ellenby", "ellenby tree farm", x$nursery)
  x$nursery <- gsub("plantsdirect", "plants direct", x$nursery)
  return(x)
} 

nursery_shape3 <- nurseryname_format(nursery_shape2)

#--add climate zone by nusery name
add_campaign_region <- function(x){
  
  x$climate_region <-ifelse(x$nursery == "darwin plant wholesalers", "Northern Territory", "imlost")
  x$climate_region <-ifelse(x$nursery == "alpine" | x$nursery == "andreasens" | x$nursery == "mangrove mountain" 
                            |x$nursery=="trees impact", "New South Wales", x$climate_region)
  x$climate_region <-ifelse(x$nursery == "fleming" | x$nursery == "speciality" | 
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
nursery_shape4 <- add_campaign_region(nursery_shape3)


##simplify hybrids (remove genus_x_hybrid)
nursery_shape4$species <- gsub("_x_", "_", nursery_shape4$species)

##genus species function
nursery_shape5 <-   species_variety_func(nursery_shape4) 


##clean species list
length(unique(nursery_shape5$genus_species))
length(unique(nursery_shape5$volume))
length(unique(nursery_shape5$species))
range(nursery_shape5$volume)


# merge species origin ----------------------------------------------------

origin <- read.csv("data/species_origin.csv")
nursery_shape6 <- merge(nursery_shape5, origin, all=TRUE)


# write clean shape file --------------------------------------------------
write.csv(nursery_shape6, "master_scripts/tree_shape.csv", row.names = FALSE)

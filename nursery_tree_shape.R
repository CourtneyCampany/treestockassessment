
# read in shape datasets --------------------------------------------------

source("functions_and_packages/size_index_format.R")

library(plyr)
shape_files <- llply(list.files(path="data/",pattern="shape",full.names=TRUE),function(filename) {
  dat=read.csv(filename, header=TRUE)
})

  shape_names<- list.files(path="data/",pattern="shape",full.names=TRUE)
  shape_names2 <- gsub("data/", "", shape_names)
  shape_names2 <- gsub(".csv", "", shape_names2)
  
  shape_names3 <- gsub("_shape", "", shape_names2)

names(shape_files) <- shape_names2


# clean and format raw data ------------------------------------

#add a column to each list element that has the nursery name
for (i in 1:18){
  shape_files[[i]]$nursery <- shape_names3[i]
}


##add climate zone
    add_campaign_region <- function(x){
      
      x$climate_region <-ifelse(x$nursery == "dph", "Northern Territory", "imlost")
      x$climate_region <-ifelse(x$nursery == "alpine" | x$nursery == "andreasens" | x$nursery == "mangrovemtn" 
                                |x$nursery=="treesimpact", "New South Wales", x$climate_region)
      x$climate_region <-ifelse(x$nursery == "fleming" | x$nursery == "speciality" | x$nursery == "ett" | 
                                  x$nursery == "mtwill", "Victoria", x$climate_region )
      x$climate_region <-ifelse(x$nursery == "ellenby"|x$nursery == "benara"|x$nursery =="arborwest", 
                                "Western Australia", x$climate_region)
      x$climate_region <-ifelse(x$nursery == "heynes"|x$nursery == "cleveland"|x$nursery =="manor"|
                                  x$nursery == "freshford"|x$nursery == "adelaideadvanced"|
                                  x$nursery == "adelaidetreefarm",
                                "South Australia", x$climate_region)
      x$climate_region <- as.factor(x$climate_region)
      print("no trees appear lost")
      return(x)
    }

shape_files <- lapply(shape_files, function(x) add_campaign_region(x))    

##i need to remove first nursery visit (alpine) as its data is in a different format
shape_data <- shape_files[-3]    
    
##simplify hybrids (remove genus_x_hybrid)
for (i in 1:17){
  shape_data[[i]]$species <- gsub("_x_", "_", shape_data[[i]]$species)
}

## genus-species-variety function
shape_data <- lapply(shape_data, function(x) species_variety_func(x))

## reformat nursery names

    nurseryname_format <- function(x) {
      x$nursery <- gsub("mangrovemtn", "mangrove mountain", x$nursery)
      x$nursery <- gsub("treesimpact", "trees impact", x$nursery)
      x$nursery <- gsub("fleming", "flemings", x$nursery)
      x$nursery <- gsub("mtwill", "mt william", x$nursery)
      x$nursery <- gsub("dph", "darwin plant wholesalers", x$nursery)
      x$nursery <- gsub("ett", "established tree transplanters", x$nursery)
      x$nursery <- gsub("adelaideadvanced", "adelaide advanced", x$nursery)
      x$nursery <- gsub("adelaidetreefarm", "adelaide tree farm", x$nursery)
      x$nursery <- gsub("ellenby", "ellenby tree farm  tree farm", x$nursery)
      return(x)
    } 

shape_files <- lapply(shape_files, function(x) nurseryname_format(x))   


# extract shape and leaf data seperately--------------------------------------

shape_format <- function(x) {

  params <- x[, c(1:8, 15:16)]
  params$crownlength <- with(params, canopy_top - canopy_bottom)
  print("successfully calculated crown size")
  
  leaves <- x[, c(1:4, 9:16)]
  
  return(list(params, leaves))
}

shape_clean <- lapply(shape_data, function(x) shape_format(x))


##merge param and leaf datasets seperately
leaf_data <- lapply(shape_clean, function(y){y[[2]]})

shape_data <- lapply(shape_clean, function(x) {x[[1]]})

library(plyr)
treeshape <-rbind.fill(shape_data)
leafmassarea <- rbind.fill(leaf_data)


### clean species names before write
test <- unique(treeshape$species)





##calculating crown volume ???
##Volume of a cylinder = π*h*r^2
##Volume of a Sphere = 4/3π*r^3
##Volume of a cone = 1/3π*r^2


x$crownvolume <- with(x, (pi* (maxbranchradius)^2 * (crownlength/3))/1000000) #units in cm
print("crown area calculated successfully")
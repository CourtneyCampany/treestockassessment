library(doBy)
library(ggplot2)

# read data ---------------------------------------------------------------

variety <- read.csv("calculated_data/species_list.csv")
genusspecies <- read.csv("calculated_data/genusspecies_list.csv")
si_means_clim <- read.csv("master_scripts/si_means_climate.csv") 
si_clim <- read.csv("master_scripts/si_climate.csv")
standard <- read.csv("reports/container_assessment.csv")
origin <- read.csv("data/species_origin.csv")

# species info plotting ---------------------------------------------------
origin_means <- summaryBy(species ~ origin + leaf_type, data=origin,FUN=length, keep.names = TRUE)
origin_means$treetype <- paste(origin_means$origin, origin_means$leaf_type, sep = " ")

theme_set(theme_bw())
science_theme <- theme(panel.border      = element_rect(color = "black"),
                       panel.grid.major  = element_blank(), 
                       panel.grid.minor  = element_blank(), 
                       legend.position   = c(.91, .91),
                       legend.title      = element_blank(),
                       legend.background = element_blank(),
                       legend.key        = element_blank(),
                       legend.key.width  = unit(2.5, "lines"),
                       legend.key.height = unit(.8, "lines"),
                       axis.ticks.length = unit(-.2, "lines"),
                       axis.text.x       = element_text(margin = margin(5)),
                       axis.text.y       = element_text(margin = margin(0, 5)),
                       axis.title.y      = element_text(margin = margin(0, 10)))

ggplot(data=origin_means, aes(x=treetype, y=species, color="lightgrey")) +
  geom_bar(colour="black", stat="identity", size=.8, fill="lightgrey") +
  xlab("") + ylab("Total Species Measured") +
  science_theme +
  theme(legend.position="none") +
  ylim(0,100)


# region counts -----------------------------------------------------------
region_counts <- summaryBy(sizeindex ~ climate_region, data=si_clim, FUN=length, keep.names = TRUE)

ggplot(data=region_counts, aes(x=reorder(climate_region, sizeindex), y=sizeindex)) +
  geom_bar(colour="black", stat="identity", size=.8, fill="lightgrey") +
  xlab("") + ylab("Total Trees Measured") +
  science_theme +
  theme(legend.position="none")


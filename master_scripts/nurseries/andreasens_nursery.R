source("functions_and_packages/plot_objects.R")

##standard
standard <- read.csv("reports/container_assessment.csv")
kemps_dat <- read.csv("data/kempscreek_sizeindex.csv")
  kemps_dat$site <- "Kemps Creek"
mangrove_dat <- read.csv("data/mangrovemtn_sizeindex.csv")
  mangrove_dat$site <- "Mangrove Mountain"
  
#need to seperate the trees with diameter tape and calculate size index
mangrove_big <- mangrove_dat[is.na(mangrove_dat$diameter2),]
mangrove_small <-  mangrove_dat[!is.na(mangrove_dat$diameter2),] 


#function for andreasens nurseries--------------------------------------------------------

andreasensformat <- function (x){
  
  x$date <- as.Date(x$date, format = "%m/%d/%Y", tz="AEST")
  print("date conversion worked")

  #need new id at Andreseans (batch_id may duplicate across species)
  x$batch_id2 <- paste(x$batch_id, x$species, sep="-")
  #calulate indices
  x$calliper300 <- with(x, (diameter1+diameter2)/2)
  x$rcd <- with(x, (rcd1+rcd2)/2)
  x$height_m <- x$height/100
  x$sizeindex <- with(x, height_m * calliper300)
  print("date format worked")
  
  #log data for plotting
  x$logSI <- with(x, log10(sizeindex))
  x$logvol <- with(x, log10(volume))
  x$logH <- with(x, log10(height_m))
  x$logD <- with(x, log10(calliper300))
  x$logRCD <- with(x, log10(rcd))
  print("log conversion worked")
  x2 <- x[, c("nursery","date", "species","site", "batch_id","batch_id2","volume", "calliper300","rcd", "height_m", "sizeindex", "logSI", "logvol", 
              "logH", "logD", "logRCD")]
  return(x2)
}  


#function for mangrove mountain where diameter tape was used instead of callipers-------------------------------------------
mangrovebig_format <- function(x){
  x$date <- as.Date(x$date, format = "%m/%d/%Y", tz="AEST")
  print("date conversion worked")
  
  #need new id at Andreseans (batch_id may duplicate across species)
  x$batch_id2 <- paste(x$batch_id, x$species, sep="-")
  #calulate indices
  x$calliper300 <- with(x, (diameter1/pi)*10)
  x$rcd <- with(x, (rcd1/pi)*10)
  x$height_m <- x$height/100
  x$sizeindex <- with(x, height_m * calliper300)
  print("date format worked")
  
  #log data for plotting
  x$logSI <- with(x, log10(sizeindex))
  x$logvol <- with(x, log10(volume))
  x$logH <- with(x, log10(height_m))
  x$logD <- with(x, log10(calliper300))
  x$logRCD <- with(x, log10(rcd))
  print("log conversion worked")
  x2 <- x[, c("nursery","date", "species","site","batch_id","batch_id2","volume","calliper300","rcd", "height_m", "sizeindex", "logSI", "logvol", 
              "logH", "logD", "logRCD")]
  return(x2)
}
 
#format data (merge mangrove datasets)-----------------------------------------------------------------------------------
 
mangrove_small2 <- andreasensformat(mangrove_small)
mangrove_big2 <- mangrovebig_format(mangrove_big)

mangrove_si <- rbind(mangrove_small2, mangrove_big2)
kemps_si <- andreasensformat(kemps_dat)

##save andreseasns sites data formatted
write.csv(mangrove_si, "calculated_data/mangrove_clean.csv", row.names = FALSE)
write.csv(kemps_si, "calculated_data/kemps_clean.csv", row.names = FALSE)


##species color assign--------------------------------------------------------------------------------------------------

library(RColorBrewer)
  
###large color palette
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

##Redo and combine both sites
andreasens_dat <- rbind(kemps_si, mangrove_si)

andreasensspecies <- unique(andreasens_dat$species)
andreasensspecies2 <- data.frame(species = andreasensspecies, colorspec = col_vector[1:20], stringsAsFactors = FALSE)

andreasens_si <- merge(andreasens_dat, andreasensspecies2)

write.csv(andreasens_si,"reports/andreasens_sizeindex.csv", row.names = FALSE)

##new dataframe of species volume totals
andreasans_batch <- unique(andreasens_si[c("species", "site","volume")])
andreasans_batch <- andreasans_batch[with(andreasans_batch, order(species, site,volume)),]
write.csv(andreasans_batch,"reports/andreasans_batch.csv", row.names = FALSE)


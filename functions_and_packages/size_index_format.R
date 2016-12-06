###regional functions for formatting size index data

melbs_format <- function (x){
  
  x$date <- as.Date(x$date, format = "%d/%m/%Y", tz="AEST")
  print("date conversion worked")
  
  #need new id at Andreseans (batch_id may duplicate across species)
  x$batch_id2 <- paste(x$batch_id, x$species, sep="-")
  
    #cm or inch pots to volume
    x$volume <- gsub("30cm", 15, x$volume)
    x$volume <- gsub("33cm", 18, x$volume)
    x$volume <- gsub("40cm", 35, x$volume)
    x$volume <- gsub("50cm", 65, x$volume)
    x$volume <- gsub("65in",1500, x$volume)
    x$volume <- gsub("95in",2500, x$volume)
    
    x$volume <- as.numeric(x$volume)
  
  ###need to seperate dfr in function whether used diameter tape or not, then caluclate parameters and remerge
  dat1 <- x[!is.na(x$diameter2),]
  dat2 <-  x[is.na(x$diameter2),]
  
  #calulate indices seperately for each dat1/2
  dat1$calliper300 <- with(dat1, (diameter1+diameter2)/2)
  dat1$rcd <- with(dat1, (rcd1+rcd2)/2)
  dat1$height_m <- dat1$height/100
  dat1$sizeindex <- with(dat1, height_m * calliper300)
  print("dat1 format worked")
  
  dat2$calliper300 <- with(dat2, (diameter1/pi)*10)
  dat2$rcd <- with(dat2, (rcd1/pi)*10)
  dat2$height_m <- dat2$height/100
  dat2$sizeindex <- with(dat2, height_m * calliper300)
  
  print("dat2 format worked")
  
  #remerge and log data for plotting
  dat3 <- rbind(dat1[,c("nursery", "date", "species", "batch_id", "batch_id2","volume","calliper300","rcd", "height_m", "sizeindex")],
                dat2[,c("nursery" ,"date", "species", "batch_id", "batch_id2","volume","calliper300","rcd", "height_m", "sizeindex")])
  print("dat 1format worked")
  
  dat3$logSI <- with(dat3, log10(sizeindex))
  dat3$volume <- as.numeric(dat3$volume)
  dat3$logvol <- with(dat3, log10(volume))
  dat3$logH <- with(dat3, log10(height_m))
  dat3$logD <- with(dat3, log10(calliper300))
  dat3$logRCD <- with(dat3, log10(rcd))
  print("log conversion worked")
  dat4 <- dat3[, c("nursery", "date", "species", "batch_id", "batch_id2","volume", "calliper300",
                   "rcd", "height_m", "sizeindex", "logSI", "logvol", "logH", "logD", "logRCD")]
  return(dat4)
}  


##darwin-----------------------------------------------------------------------------------------------------------------------
darwin_format <- function (x){
  
  x$date <- as.Date(x$date, format = "%d/%m/%Y", tz="AEST")
  print("date conversion worked")
  
  #need new id at Andreseans (batch_id may duplicate across species)
  x$batch_id2 <- paste(x$batch_id, x$species, sep="-")
  x$volume <- as.numeric(x$volume)
  
  ###need to seperate dfr in function whether used diameter tape or not, then caluclate parameters and remerge
  dat1 <- x[!is.na(x$diameter2),]
  dat2 <-  x[is.na(x$diameter2),]
  
  #calulate indices seperately for each dat1/2
  dat1$calliper300 <- with(dat1, (diameter1+diameter2)/2)
  dat1$rcd <- with(dat1, (rcd1+rcd2)/2)
  dat1$height_m <- dat1$height/100
  dat1$sizeindex <- with(dat1, height_m * calliper300)
  print("dat1 format worked")
  
  dat2$calliper300 <- with(dat2, (diameter1/pi)*10)
  dat2$rcd <- with(dat2, (rcd1/pi)*10)
  dat2$height_m <- dat2$height/100
  dat2$sizeindex <- with(dat2, height_m * calliper300)
  
  print("dat2 format worked")
  
  #remerge and log data for plotting
  dat3 <- rbind(dat1[,c("nursery", "date", "species", "batch_id", "batch_id2","volume","calliper300","rcd", "height_m", "sizeindex")],
                dat2[,c("nursery", "date", "species", "batch_id", "batch_id2","volume","calliper300","rcd", "height_m", "sizeindex")])
  print("dat 1format worked")
  
  dat3$logSI <- with(dat3, log10(sizeindex))
  dat3$volume <- as.numeric(dat3$volume)
  dat3$logvol <- with(dat3, log10(volume))
  dat3$logH <- with(dat3, log10(height_m))
  dat3$logD <- with(dat3, log10(calliper300))
  dat3$logRCD <- with(dat3, log10(rcd))
  print("log conversion worked")
  dat4 <- dat3[, c("nursery", "date", "species", "batch_id", "batch_id2","volume", "calliper300",
                   "rcd", "height_m", "sizeindex", "logSI", "logvol", "logH", "logD", "logRCD")]
  return(dat4)
} 

#------in or out of range
doesfit_func <- function(x) {
  x$balanced <- ifelse(x$volume == 5 & x$sizeindex >=7.7 & x$sizeindex <=11.3, "pass", "fail")
  x$balanced <- ifelse(x$volume == 14 & x$sizeindex >=18.6 & x$sizeindex <=27.5, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 15 & x$sizeindex >=19.7 & x$sizeindex <=29, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 18 & x$sizeindex >=23 & x$sizeindex <=34, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 20 & x$sizeindex >=24 & x$sizeindex <=37, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 25 & x$sizeindex >=31 & x$sizeindex <=45, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 30 & x$sizeindex >=36 & x$sizeindex <=53, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 35 & x$sizeindex >=41 & x$sizeindex <=61, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 45 & x$sizeindex >=51 & x$sizeindex <=75, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 50 & x$sizeindex >=56 & x$sizeindex <=82, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 65 & x$sizeindex >=70 & x$sizeindex <=113, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 75 & x$sizeindex >=79 & x$sizeindex <=117, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 100 & x$sizeindex >=102 & x$sizeindex <=150, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 150 & x$sizeindex >=144 & x$sizeindex <=212, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 200 & x$sizeindex >=185 & x$sizeindex <=272, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 400 & x$sizeindex >=330 & x$sizeindex <=494, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 500 & x$sizeindex >=407 & x$sizeindex <=599, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 600 & x$sizeindex >=476 & x$sizeindex <=700, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 750 & x$sizeindex >=577 & x$sizeindex <=849, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 800 & x$sizeindex >=610 & x$sizeindex <=898, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 1000 & x$sizeindex >=739 & x$sizeindex <=1087, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 1200 & x$sizeindex >=865 & x$sizeindex <=1272, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 1500 & x$sizeindex >=1048 & x$sizeindex <=1542, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 2000 & x$sizeindex >=1343 & x$sizeindex <=1975, "pass", x$balanced)
  x$balanced <- ifelse(x$volume == 2500 & x$sizeindex >=1627 & x$sizeindex <=2393, "pass", x$balanced)
  return(x)
}

#--add climate zone
add_campaign_region <- function(x){
  
  x$climate_region <-ifelse(x$nursery == "dph", "Northern Territory", "imlost")
  x$climate_region <-ifelse(x$nursery == "alp" | x$nursery == "a_kc" | x$nursery == "a_mm" 
                            |x$nursery=="treesimpact", "New South Wales", x$climate_region)
  x$climate_region <-ifelse(x$nursery == "flem" | x$nursery == "spec" | x$nursery == "ett" | 
                            x$nursery == "mtwil", "Victoria", x$climate_region )
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
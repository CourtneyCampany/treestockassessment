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
  dat3 <- rbind(dat1[,c("date", "species", "batch_id", "batch_id2","volume","calliper300","rcd", "height_m", "sizeindex")],
                dat2[,c("date", "species", "batch_id", "batch_id2","volume","calliper300","rcd", "height_m", "sizeindex")])
  print("dat 1format worked")
  
  dat3$logSI <- with(dat3, log10(sizeindex))
  dat3$volume <- as.numeric(dat3$volume)
  dat3$logvol <- with(dat3, log10(volume))
  dat3$logH <- with(dat3, log10(height_m))
  dat3$logD <- with(dat3, log10(calliper300))
  dat3$logRCD <- with(dat3, log10(rcd))
  print("log conversion worked")
  dat4 <- dat3[, c("date", "species", "batch_id", "batch_id2","volume", "calliper300",
                   "rcd", "height_m", "sizeindex", "logSI", "logvol", "logH", "logD", "logRCD")]
  return(dat4)
}  

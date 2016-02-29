prelim <- read.csv("data/nursery_data.csv")
prelim$SI <- with(prelim, height * caliper_30cm)
prelim$logSI <- with(prelim, log10(SI))
prelim$logvol <- with(prelim, log10(container_l))
prelim$d2h <- with(prelim, caliper_30cm^2 * height)

standard <- read.csv("data/si_standard.csv")
standard$logminSI <- with(standard, log10(min_size_index))
standard$logmaxSI <- with(standard, log10(max_size_index))
standard$logvol <- with(standard, log10(container_volume))


#nursery datat
appdata <- prelim[, c(1,8:10,12:13) ] 
##reorder for shiny
appdata2 <- appdata[, c(5:6,1:4)]


##plotbits
silab <- expression(Size~index~range~~(calliper~x~height)) 
cols <- c("forestgreen", "gold", "red4")

minx <- min(standard$logvol)
maxx <- max(standard$logvol)
minsimin <- min(standard$logminSI)
maxsimin <- max(standard$logminSI)
minsimax <- min(standard$logmaxSI)
maxsimax <- max(standard$logmaxSI)

greytrans <- alpha("grey", alpha=0.5)
blacktrans <- alpha("black", alpha=0.5)
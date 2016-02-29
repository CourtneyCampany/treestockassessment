###this will be the basis for the interactive pltos with size index.

###I first want to create a polygon over the SI range that will not be interactive.  Then i will build the
###interactive bits with the live data


##start base plots


source("functions_and_packages/plot_objects.R")


##Test size index with prelim data


##standard
standard <- read.csv("data/container_assessment.csv")
  standard$logminSI <- with(standard, log10(min_size_index))
  standard$logmaxSI <- with(standard, log10(max_size_index))
  standard$logvol <- with(standard, log10(container_volume))

###nursery data
prelim <- read.csv("data/prelim_data.csv")
prelim$SI <- with(prelim, height * caliper_30cm)

prelim$logSI <- with(prelim, log10(SI))
prelim$logvol <- with(prelim, log10(container_l))
prelim$d2h <- with(prelim, caliper_30cm^2 * height)


##plot bits
cols <- c("green", "yellow", "red")

minx <- min(standard$logvol)
maxx <- max(standard$logvol)
minsimin <- min(standard$logminSI)
maxsimin <- max(standard$logminSI)
minsimax <- min(standard$logmaxSI)
maxsimax <- max(standard$logmaxSI)
library(scales)

greytrans <- alpha("grey", alpha=0.5)

###plotting-------------------------------------------------------------------------------------------------------------
library(magicaxis)

par(mar=c(6,7,2,2),cex.axis=1.4, cex.lab=1.75,las=0,mgp=c(4.5,1,0))
plot(logSI ~ logvol, data=prelim, xlab="Container volume (L)", ylab=silab,
     axes=FALSE, xlim=c(1,3.5),ylim=c(1, 3.5),type='n')
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)

#add assessment range
polygon(x=c(minx,minx,maxx,maxx), y=c(minsimin, minsimax, maxsimax,maxsimin), lwd=2, col=greytrans )
box()



# selectInput("growthrate", "Select a Growth Rate:",
#             levels(prelim$growth_rate),
#             selected = levels(prelim$growth_rate))),



# trees.sset <- reactive(subset(prelim, growthrate %in% input$growthrate))
# 
# #plot generation
# output$custom.plot <- renderPlot({
#   
#   #generate plot
#   y <- prelim$growthrate[prelim$growthrate == input$growthrate, "growthrate"]
#   x <- prelim$logvol
#   plot(x,y, xlab="Container volume (L)", ylab="cool label",
#        axes=FALSE, xlim=c(1,3.5),ylim=c(1, 3.5),xlim=c(1,3.5),ylim=c(1, 3.5), pch=16)
#   magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
#   box()




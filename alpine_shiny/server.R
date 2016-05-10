


shinyServer(function(input, output, session) {
  
  
  species_ss<- reactive({
    subset(appdat, species %in% input$whichspecies)[,-c(1,4)]
    })
  species.color <- reactive({
    unique(subset(appdat, species %in% input$whichspecies)[,-c(2,3)])
    })
  
  output$plot1 <- renderPlot({
    
    par(mar=c(6,7,2,2),cex.axis=1.4, cex.lab=1.75,las=0,mgp=c(4.5,1,0))
    plot(appdat$logSI~ appdat$logvol,
         axes=FALSE, xlim=c(.5,3.75), ylim=c(.5,3.5),
         pch = 1, cex = 2, ylab=silab,
         xlab="Container volume (L)")
    magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
    
    #add assessment range
    polygon(x=c(minx,minx,maxx,maxx), y=c(minsimin, minsimax, maxsimax,maxsimin), lwd=2,lty=2, col=greytrans )
    box()
    
    ###reactive here for species colors
    points(species_ss()[[1]]~ species_ss()[[2]], bg=species.color()[,2], pch=21, cex=2)
  })
  
  output$brushedPoints <- renderPrint({
    rows <- brushedPoints(appdat[,c(1:3)], input$plot_brush,xvar = "logvol", yvar = "logSI")
    cat("Selected points:\n")
    print(rows)
  })
})
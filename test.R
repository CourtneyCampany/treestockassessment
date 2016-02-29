selectInput('xcol', 'X Variable', names(appdata2)),
selectInput('ycol', 'Y Variable', names(appdata2),
            selected=names(appdata2)[[2]])




shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    appdata2[, c(input$ycol, input$xcol)]
  })
  
  output$plot1 <- renderPlot({
    par(mar=c(6,7,2,2),cex.axis=1.4, cex.lab=1.75,las=0,mgp=c(4.5,1,0))
    plot(selectedData(),
         axes=FALSE, xlim=c(1,3.5),ylim=c(1, 3.5),
         pch = 19, cex = 2, ylab=silab,
         xlab="Container volume (L)", col=cols)
    magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
    
    #add assessment range
    polygon(x=c(minx,minx,maxx,maxx), y=c(minsimin, minsimax, maxsimax,maxsimin), lwd=2, col=greytrans )
    box()
    
    ###the goal is to add the reactive here (cols for different tree types, added onto base points)
    ##1. growth rate
    
    
  })
  
})

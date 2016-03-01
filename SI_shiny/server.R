


shinyServer(function(input, output, session) {

  # Combine the selected variables into a new data frame
  gr_ss<- reactive(subset(appdata2, growth_rate %in% input$whichgrowth)[,-c(3:9)]) 
  growth.color <- reactive(unique(subset(appdata2, growth_rate %in% input$whichgrowth)[,c(3,7)]))
  
  native_ss<- reactive(subset(appdata2, origin %in% input$whichorigin)[,-c(3:9)]) 
  origin.color <- reactive(unique(subset(appdata2, origin %in% input$whichorigin)[,c(3,8)]))
  
  foliage_ss<- reactive(subset(appdata2, type %in% input$whichleaf)[,-c(3:9)]) 
  leaftype.color <- reactive(unique(subset(appdata2, type %in% input$whichleaf)[,c(3,9)]))
  
  
  output$plot1 <- renderPlot({
    par(mar=c(6,7,2,2),cex.axis=1.4, cex.lab=1.75,las=0,mgp=c(4.5,1,0))
    plot(appdata2$logSI~ appdata2$logvol,
         axes=FALSE, xlim=c(1,3.5),ylim=c(1, 3.5),
         pch = 1, cex = 2, ylab=silab,
         xlab="Container volume (L)")
    magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
    
    #add assessment range
    polygon(x=c(minx,minx,maxx,maxx), y=c(minsimin, minsimax, maxsimax,maxsimin), lwd=2,lty=2, col=greytrans )
    box()
    
    ###the goal is to add the reactive here (cols for different tree types, added onto base points)
    ##1. growth rate
    points(gr_ss()[[1]]~ gr_ss()[[2]], bg=growth.color()[,2], pch=21, cex=2)
    points(native_ss()[[1]]~ native_ss()[[2]], bg=origin.color()[,2], pch=21, cex=2)
    points(foliage_ss()[[1]]~ foliage_ss()[[2]], bg=leaftype.color()[,2], pch=21, cex=2)
    
  })
  
  output$brushedPoints <- renderPrint({
    rows <- brushedPoints(appdata2, input$plot_brush,xvar = "logvol", yvar = "logSI")
    cat("Selected points:\n")
    print(rows)
  })

  
})

  
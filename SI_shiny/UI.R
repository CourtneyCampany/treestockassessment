library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel('Evaluating Tree Size Index'),
  
  sidebarPanel(

    ##type selection
    checkboxGroupInput("whichgrowth", "Pick a Growth Rate:",
                      c("fast","moderate","slow" )),
    
    checkboxGroupInput("whichorigin", "Native or Non_Native:",
                       c("native","nonnative" )),
    
    checkboxGroupInput("whichleaf", "Foliage type:",
                       c("deciduous","evergreen" ))
    
  ),
  
  
  
  mainPanel(
    plotOutput('plot1', brush=brushOpts("plot_brush",resetOnNew = TRUE, fill = "blue", opacity=.5, stroke="black"),
               width=800, height=600),
    verbatimTextOutput("brushedPoints")
    )
))

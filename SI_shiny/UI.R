library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel('Evaluating Tree Size Index'),
  
  sidebarPanel(

    ##type selection
    checkboxGroupInput("whichgrowth", "Pick a Growth Rate:",
                      c("fast","moderate","slow" )),
    
    checkboxGroupInput("whichorigin", "Native or Non_Native:",
                       c("native","non-native" )),
    
    checkboxGroupInput("whichleaf", "Foliage type:",
                       c("deciduous","evergreen" ))
    
  ),
  
  
  
  mainPanel(
    plotOutput('plot1')
  )
))

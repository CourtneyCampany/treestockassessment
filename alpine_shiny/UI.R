

shinyUI(pageWithSidebar(
  
  headerPanel('Evaluating Tree Size Index'),
  sidebarPanel(
    ##type selection
    checkboxGroupInput("whichspecies", "Pick a Species:",species)
  ),
  
  mainPanel(
    plotOutput('plot1'
               ,brush=brushOpts("plot_brush",resetOnNew = TRUE, fill = "blue", opacity=.5, stroke="black"),
               width=800, height=600), verbatimTextOutput("brushedPoints")
  )
  ))


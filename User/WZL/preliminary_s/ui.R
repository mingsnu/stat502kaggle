shinyUI(
    fluidPage(    
    
    titlePanel("Preliminary analysis"),
    
    sidebarLayout(      
      
      sidebarPanel(
        selectInput('var', 'Available variable', var.name)
      ),
            mainPanel(
        plotOutput("phonePlot")  
      )
      
    )
  )
)

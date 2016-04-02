library(shiny)

# Define a server for the Shiny app

shinyServer(function(input, output) {

    train.hist=reactive({
      num= which(var.name == input$var)
     preliminary[[num]]$hist.i
        })
    all.hist=reactive({
      num = which(var.name == input$var)
     preliminary.all[[num]]$hist.i
    })
    train.relation=reactive({
      num = which(var.name == input$var)
     preliminary[[num]]$plot.y
    })
  # result.sensor = unique(result.sensor)
  # Fill in the spot we created for a plot

  
  output$phonePlot <- renderPlot({
    
    # Render a barplot
    p=train.hist()
     
    p1 = all.hist()
    p2 = train.relation()
   

      grid.arrange(p,p1,p2, layout_matrix = cbind(c(1,1,3,3,3), c(2,2,3,3,3)))
   
  },height = 800, width = 1000)
})
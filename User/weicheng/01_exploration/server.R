
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

shinyServer(function(input, output) {
  ######## tab1 #######
  output$plot1 = renderPlot({
    plot(trn.unique.length, ylim=c(0, input$num1))
  })
  
  output$table1 =  DT::renderDataTable({
    apply(trn[, which(trn.unique.length == input$num2)], 2, function(x) sort(unique(x)))
  }, selection = list(target = 'column'))
  
  # current length table
  clt = reactive({
    trn.unique.length[which(trn.unique.length == input$num2)]
  })
  
  output$text1 = renderPrint({
    cat("Selected variables are:\n")
    nms = names(clt())
    cat(paste0("'", nms[input$table1_columns_selected + 1], "'", collapse = ", "))
  })
  
  ######## tab2 ########
  output$table2 = DT::renderDataTable({
    input$reset2
    data.frame(ID = 1:length(trn.unique.length), var = names(trn.unique.length), value = unname(trn.unique.length))
  }, options = list(pageLength = 25), selection = list(target = 'row'), rownames = FALSE
  )
  
  output$plot2 = renderPlot({
    req(input$table2_rows_selected)
    idx = sample(1:nrow(trn))
    nms = names(trn.unique.length)[input$table2_rows_selected]
    if(length(nms) > 1)
      plot(trn[idx, nms[1]], trn[idx, nms[2]], pch=19, col=rgb(red=0.2, green=0.2, blue=1.0, alpha=0.2), xlab=nms[1], ylab=nms[2])
  })
})

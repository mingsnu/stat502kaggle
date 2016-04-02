
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(DT)
library(dplyr)

shinyServer(function(input, output) {
  ######## tab1 #######
  output$plot1 = renderPlot({
    plot(trn.unique.length, ylim=c(0, input$num1))
  })
  
  output$table1 =  DT::renderDataTable(
    apply(trn[, which(trn.unique.length == input$num2)], 2, 
                        function(x) sort(unique(x), na.last = TRUE)),
    selection = list(target = 'column'))
  
  output$table1_1 =  DT::renderDataTable(
    {
      alldat = lapply(which(trn.unique.length == input$num2), function(j) sort(unique(dat[, j])))
      attributes(alldat) = list(names = names(alldat),
                            row.names=1:max(sapply(1:length(alldat), function(i) length(alldat[[i]]))), class='data.frame')
      alldat
    },
    selection = list(target = 'column'))
  
  
  # current length table
  clt = reactive({
    trn.unique.length[which(trn.unique.length == input$num2)]
  })
  colSelected = reactive({
    nms = names(clt())
    nms[input$table1_columns_selected + 1]
  })
  
  output$text1 = renderPrint({
    cat("Selected variables are:\n")
    cat(paste0("'", colSelected(), "'", collapse = ", "))
  })
  
  output$text2 = renderPrint({
    cat("Unselected variables in the table are:\n")
    cat(paste0("'", dplyr::setdiff(names(clt()), colSelected()), "'", collapse = ", "))
    
  })
  
  ######## tab2 ########
  output$table2 = DT::renderDataTable({
    input$reset2
    data.frame(ID = 1:length(trn.unique.length), var = names(trn.unique.length), value = unname(trn.unique.length))
  }, options = list(pageLength = 25), selection = list(target = 'row'), rownames = FALSE
  )
  
  output$plot2 = renderPlot({
    req(input$table2_rows_selected)
    nms = names(trn.unique.length)[input$table2_rows_selected]
    if(length(nms) > 1)
      plot(trn[, nms[1]], trn[, nms[2]], pch=19, 
           col=c("#FF333333", "#3333FF33")[trn$TARGET+1], xlab=nms[1], ylab=nms[2])
  })
})

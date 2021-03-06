
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

shinyUI(navbarPage("LSWZZ",
                   tabPanel("Data type",
                            # Sidebar with a slider input for number of bins
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("num1", "ylim: (2 - 76020)", value = 76020, 
                                             min=1, max = 76020, step=1),
                                numericInput("num2", "Length of unique values to review", value=2, min=2, step=1),
                                p(strong("Selected variables are:")),
                                verbatimTextOutput("text1"),
                                p(strong("Unselected variables in the table are:")),
                                verbatimTextOutput("text2")
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput("plot1"),
                                h3("Training data unique value table (empty cell is NA)"),
                                DT::dataTableOutput("table1"),
                                h3("Testing + Training data unique value table"),
                                DT::dataTableOutput("table1_1")
                              )
                            )
                   ),
                   tabPanel("Correlation",
                            sidebarLayout(
                              sidebarPanel(
                                actionButton('reset2', "Clear selections", style="margin: 5px 5px 20px 5px"),
                                DT::dataTableOutput("table2")
                              ),
                              mainPanel(
                                plotOutput("plot2")
                              )
                            )
                            )
))

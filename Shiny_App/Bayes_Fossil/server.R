#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("FossilModel.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #samplesX <- reactive(rnorm(input$n, input$meanX, input$sdX))
    #samplesY <- reactive(rnorm(input$n, input$meanY, input$sdY))
    
    fossil <- reactive(getFossilData(input$percData/100))
    
    output$dataPlot <- renderPlot({
        plotFossilData(fossil())
    })

})

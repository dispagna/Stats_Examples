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
    
    fossil <- reactive(getFossilData(input$percData/100))
    std <- reactive(input$checkbox)

    output$dataPlot <- renderPlot({
        plotFossilData(fossil())
    })
    
    mdl <- eventReactive(input$updateButton, {
        withProgress(message="Fitting model...please be patient...", style="old",
                     {
                         getModel(fossil(), input$mu, input$sigma, input$lambda, input$k)
                    })
    })
    
    output$priorPlot <- renderPlot({
        plotPred(mdl()$prior)
    })

    output$mcmcChains <- renderPlot({
        plotChains(mdl()$post)
    })
    
    output$modelSummary <- renderPrint({
        getSummary(mdl()$post)
    })
    
    output$postPlot <- renderPlot({
        plotPred(mdl()$post)
    })
    
    observeEvent(req(input$checkbox==FALSE), {
            showModal(modalDialog(
                title = "Warning",
                "Model fitting is significantly slower if data isn't standardized.",
                easyClose = TRUE
            ))
    })
})

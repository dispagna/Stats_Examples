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

    output$dataPlot <- renderPlot({
        plotFossilData(fossil())
    })
    
    mdl_prior <- eventReactive(input$updateButton, {
        
    }
    )
    
    mdl <- eventReactive(input$updateButton, {
        withProgress(message="Calculating model...this may take a few minutes...", style="old",
                     {
                         getModel(fossil(), input$mu, input$sigma, input$lambda, input$k)
                         # incProgress(amount=0.5, 
                         #             message = "Calculating posterior predictive...this make take a few minutes...")
                         # post <- getPostPred(fossil(), input$mu, input$sigma, input$lambda, input$k)
                         # list(prior=prior, post=post)
                    })
    })
    
    output$priorPlot <- renderPlot({
        plotPred(mdl()$prior, fossil())
    })

    output$mcmcChains <- renderPlot({
        plotChains(mdl()$post)
    })
    
    output$modelSummary <- renderPrint({
        getSummary(mdl()$post)
    })
    
    output$postPlot <- renderPlot({
        plotPred(mdl()$post, fossil())
    })
})

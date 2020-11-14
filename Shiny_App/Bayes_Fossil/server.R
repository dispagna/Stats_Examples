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

# custom tryCatch to return result and warnings from
# http://stackoverflow.com/a/24569739/2271856
myTryCatch <- function(expr) {
    warn <- err <- NULL
    value <- withCallingHandlers(
        tryCatch(expr, error=function(e) {
            err <<- e
            NULL
        }), warning=function(w) {
            warn <<- append(warn, w)
            invokeRestart("muffleWarning")
        })
    list(value=value, warning=warn, error=err)
}

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
                         res <- myTryCatch(getModel(fossil(), input$mu_intercept, input$sigma_intercept, 
                                      input$lambda, input$lambda_w, input$k))
                         if (!is.null(res$warning)) {
                                      showModal(modalDialog(
                                          title = "Warning",
                                          res$warning,
                                          easyClose = TRUE
                                      ))
                         }
                         if (!is.null(res$error)) {
                             showModal(modalDialog(
                                 title = "Error",
                                 res$error,
                                 easyClose = TRUE
                             ))
                         }
                         res$value
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

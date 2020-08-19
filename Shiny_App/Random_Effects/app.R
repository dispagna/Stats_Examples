#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("fixedEffects.R")
source("mixedEffects.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fixed vs. Random Effects"),
    
    p("Some stuff here..."),
    
    radioButtons("fixedEffect", label=NULL, inline=TRUE,
                 choiceNames = c("Fixed", "Random"), choiceValues = c(TRUE, FALSE)),

    fluidRow(column(3,sliderInput("sdCheerios", "Cheerios", min=1, max=4, value=2, step=0.1),
                               sliderInput("sdFrootLoops", "Froot Loops", min=1, max=4, value=2, step=0.1),
                               sliderInput("sdRaisinBran", "Raisin Bran", min=1, max=4, value=2, step=0.1),
                               sliderInput("n", "Number of Boxes per Brand", min=3, max=15, value=5, step=1)),
             column(9,plotOutput("distPlot", height = "200px"),
                      verbatimTextOutput("aov"))),
    
    p("Some more stuff here...")
    
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    meanCheerios <- 9
    meanFrootLoops <- 13
    meanRaisinBran <- 15
    
    # Get random samples
    samplesCheerios <- reactive(rnorm(input$n, meanCheerios, input$sdCheerios))
    samplesFrootLoops <- reactive(rnorm(input$n, meanFrootLoops, input$sdFrootLoops))
    samplesRaisinBran <- reactive(rnorm(input$n, meanRaisinBran, input$sdRaisinBran))
    
    res <- reactive(anovaRandom(samplesCheerios(), 
                                samplesFrootLoops(), 
                                samplesRaisinBran())
    )
    
    output$distPlot <- renderPlot({ 
        if (input$fixedEffect)
            plotFixed(meanCheerios, input$sdCheerios, samplesCheerios(),
                      meanFrootLoops, input$sdFrootLoops, samplesFrootLoops(),
                      meanRaisinBran, input$sdRaisinBran, samplesRaisinBran())
        else
            plotRandom(samplesCheerios(), samplesFrootLoops(), samplesRaisinBran(),
                       res()$overallMean, res()$overallVar)
        })
        
    output$aov <- renderPrint({ 
        if (input$fixedEffect)
            summary(anovaFixed(samplesCheerios(), samplesFrootLoops(), samplesRaisinBran()))
        else
        {
            summary(res()$mdl)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

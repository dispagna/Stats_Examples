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
    
    p("In this example, I try to give a more intuitive understanding of 
      the differences between fixed and random effects without diving into the equations
      for expected mean squares, sum of squares, etc. There are plenty of references which
      show how to derive those equations; for me the challenge was understanding the 
      differences at a conceptual level when first learning about analyis of variance
      (ANOVA)."),
    p("Imagine you bought multiple boxes of three brands of cereal--Cheerios, Froot Loops
      and Raisin Bran. Further, imagine you have a device which can measure the actual
      sugar content per serving in each box (as opposed to the value on the
      nutritional label). You could do either a fixed effect or a random effect ANOVA on
      the measured sugar content data. Which do you choose to do and why?"),
    
    fluidRow(column(3, radioButtons("fixedEffect", label=NULL, inline=TRUE,
                 choiceNames = c("Fixed", "Random"), choiceValues = c(TRUE, FALSE))),
             column(9, uiOutput("question")),
             ),
    
    fluidRow(hr()),
    
    fluidRow(column(3,p()),
             column(9, uiOutput("details"))
             ),
    
    fluidRow(column(3, helpText("Adjust the standard deviation within brand or number of boxes below."),
                    sliderInput("sdCheerios", "Cheerios", min=1, max=4, value=4, step=0.1),
                               sliderInput("sdFrootLoops", "Froot Loops", min=1, max=4, value=4, step=0.1),
                               sliderInput("sdRaisinBran", "Raisin Bran", min=1, max=4, value=4, step=0.1),
                               sliderInput("n", "Number of Boxes per Brand", min=3, max=15, value=3, step=1)),
             column(9,plotOutput("distPlot", height = "200px"),
                    helpText("ANOVA Output"),
                      verbatimTextOutput("aov"))),
    
    textOutput("explanation")
    
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
    
    output$question <- renderUI({
        if(input$fixedEffect)
            HTML(paste(
            "Treating Brand as a fixed effect means you are asking the question ",
            "<B>Is there
             a statistically significant difference in the average sugar content
             between Cheerios, Froot Loops and Raisin Bran?</B>"
            ))
        else
            HTML(paste(
                "Treating Brand as a random effect means you are asking the question ",
                "<B>How much of the variance in sugar content for <i>all</i> cereal brands 
                is due to variation within each brand versus variation between brands?</B>"
            ))    
        })
    
    output$details <- renderUI({
        if(input$fixedEffect)
            HTML(paste(
                "Observe that with a small number of samples and large variance in sugar content
            for each brand it is difficult to determine whether or not there is a difference.
            As you increase the sample size and decrease the variance it becomes easier to
            see that the mean sugar content is different between the three brands."
            ))
        else
            HTML(paste(
                "In the ANOVA output, the Brand random effect is the variance
                <i>between</i> brands and the Residuals random effect is the variance
                <i>within</i> brands. As you change the number of samples and variance
                for each brand, observe how that affects the variance between and within
                brands. Also, notice the fixed effect Intercept term
                at the very bottom of the ANOVA output--
                that is the estimated mean sugar content for all brands of cereal."
            ))    
    })
    
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

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

source("plotTwoNorms.R")
source("tTests.R")
source("lrTest.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Comparing Two Means"),
    withMathJax(),
    
    fluidRow(
        column(6,
               # General Description
               p("A t-test is the typical statistical test used to compare two population
      means. However, it's instructive to see that this same comparison can
      be thought of in terms of Analysis of Variance (ANOVA), linear regression,
       or a Bayesian estimation problem."),
               
               p("Adjust the slider bars to change the mean, standard 
               deviation or number of random 
                    samples drawn from each population, and then observe
                 how these changes affect the results obtained from each method."),
               
               fluidRow(
                   column(4,
                          # Inputs for X
                          sliderInput("meanX", "X Mean", min=-5, max=5, value=0),
                          sliderInput("sdX", "X Std Dev", min=0.5, max=10, value=1)
                   ),
                   column(4,
                          # Inputs for y
                          sliderInput("meanY", "Y Mean", min=-5, max=5, value=1),
                          sliderInput("sdY", "Y Std Dev", min=0.5, max=10, value=1)
                   ),
                   column(4,
                          # Number of samples
                          sliderInput("n", "Number of Samples", min=5, max=100, value=30))
               ),
               
               fluidRow(plotOutput("distPlot", height="200px"))
               ),
        column(6,
               p("Select each tab to see the different ways this comparison can
       be formulated."),
               
               tabsetPanel(
                   tabPanel("t-Test",
                            p("There are two types of t-tests, pooled and 
                   Welch's.  The pooled t-test is appropriate when
                   the population variances are approximately equal and is generally 
                   robust if there aren't many outliers. Welch's 
                   test is appropriate if the variances are not equal; 
                   however it gives the same results as the pooled test 
                   when the variances and sample sizes are equal. Results 
                   for a two-sided test with 95% confidence for both methods are
                   shown below."),
                            
                            verbatimTextOutput("tTestResults")
                   ),
                   tabPanel("ANOVA"),
                   tabPanel("Linear Regression",
                            helpText("If we use an indicator
                              variable \\(\\begin{equation}
                                       x_i = 
                                           \\begin{cases}
                                                0, & X \\\\
                                                1, & Y
                                            \\end{cases}
                                    \\end{equation}\\)"),
                            
                            helpText("then we can formulate this as a linear
                              regression problem
                              $$\\mu_i = \\alpha + \\beta * x_i$$"),
                            
                            helpText("where testing for \\(\\beta=0\\) is
                                     equivalent to testing if the two population
                                     means are equal."),
                            
                            em("Note that for two groups with equal variances,
                                      the F-test statistic = t-test statistic",
                                      tags$sup(2), "and the p-values are identical."),
                            
                            verbatimTextOutput("lrResults"),
                            plotOutput("lrPlot")
                   ),
                   tabPanel("Bayesian Estimation")
               )
               )
    )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Get random samples
    samplesX <- reactive(rnorm(input$n, input$meanX, input$sdX))
    samplesY <- reactive(rnorm(input$n, input$meanY, input$sdY))
    
    output$distPlot <- renderPlot({
        plotTwoNorms(input$meanX, input$sdX, samplesX(),
                     input$meanY, input$sdY, samplesY())
    })
    
    output$tTestResults <- renderPrint({
        tTests(samplesX(), samplesY())
    })
    
    output$lrPlot <- renderPlot({
        lrPlot(samplesX(), samplesY())
    })
    
    output$lrResults <- renderPrint({
        lrResults(samplesX(), samplesY())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Comparing Two Means"),
    
    # General Description
    p("A t-test is the typical statistical test used to compare two population
      means. However, it's instructive to see that this same comparison can
      be thought of in terms of Analysis of Variance (ANOVA), linear regression,
       or a Bayesian estimation problem."),
    
    p("Adjust the slider bars to change the mean, standard 
               deviation or number of random 
                    samples drawn from each population."),

    fluidRow(
        column(4,
            # Inputs for X
            sliderInput("meanX", "X Mean", min=-5, max=5, value=0),
            sliderInput("sdX", "X Std Dev", min=0.5, max=5, value=1)
            ),
        column(4,
            # Inputs for y
            sliderInput("meanY", "Y Mean", min=-5, max=5, value=1),
            sliderInput("sdY", "Y Std Dev", min=0.5, max=5, value=1)
            ),
        column(4,
               # Number of samples
               sliderInput("n", "Number of Samples", min=5, max=100, value=30))
    ),
    
    fluidRow(plotOutput("distPlot", height="200px")),
    
    p("Select each tab to see the different ways this comparison can
       be formulated."),
    
    tabsetPanel(
        tabPanel("t-Test"),
        tabPanel("ANOVA"),
        tabPanel("Linear Regression"),
        tabPanel("Bayesian Estimation")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Get random samples
    samplesX <- reactive(rnorm(input$n, input$meanX, input$sdX))
    samplesY <- reactive(rnorm(input$n, input$meanY, input$sdY))
    
    output$distPlot <- renderPlot({
        dfX <- data.frame(lbl = "X",
                          idx = seq(input$meanX - 3*input$sdX, 
                                  input$meanX + 3*input$sdX, 
                                  0.1)) %>%
            mutate(prob = dnorm(idx, input$meanX, input$sdX))
        
        dfY <- data.frame(lbl = "Y",
                          idx = seq(input$meanY - 3*input$sdY, 
                                  input$meanY + 3*input$sdY, 
                                  0.1)) %>%
            mutate(prob = dnorm(idx, input$meanY, input$sdY))
        
        rbind(dfX, dfY) %>%
            ggplot(aes(colour=lbl)) +
            geom_line(aes(x=idx, y=prob)) +
            geom_point(data.frame(lbl="X", x=samplesX()), 
                       mapping=aes(x=x, y=0)) +
            geom_point(data.frame(lbl="Y", x=samplesY()), 
                       mapping=aes(x=x, y=0)) +
            theme(legend.title=element_blank())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simpson's Paradox and Causal Inference"),
    
    # General description
    p("When I first learned about Simpson's paradox, the definition was 
             straightforward--the conditional odds ratio is reversed from the marginal 
             odds ratio.  What was puzzling to
             me was ", em("how"), " to know whether or not to include the covariate
             in the model. Surely, it couldn't always be correct to include the covariate? 
             It turns out causal inference is the method for determining whether or not 
             to include the covariate, or conversely how to determine if the data is
             consistent with the hypothesized causal model; this is especially important
             for observational studies."
      ),
    p("This is not intended to be a complete explanation of causal inference, but rather
      just to illustrate that there is more to understanding and untangling
      Simpson's paradox.  Two great references for a more complete introduction to
      causal inference are Chapter 5 of McElreath's",
             tags$a(href="https://www.routledge.com/Statistical-Rethinking-A-Bayesian-Course-with-Examples-in-R-and-STAN/McElreath/p/book/9780367139919","Statistical Rethinking"),
             "and Pearl, et al's ",
             tags$a(href="https://www.wiley.com/en-us/Causal+Inference+in+Statistics%3A+A+Primer-p-9781119186847", "Causal Inference in Statistics: A Primer."),
             "The examples below are taken from Pearl's book."
    )

   
)

# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)

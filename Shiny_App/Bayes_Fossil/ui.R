#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Bayesian Model for Fossil Data"),
    withMathJax(),
    
    fluidRow(
        column(6,
               p("Using the slider bars, you can see the effect of changing the characteristics
                 of the priors and/or amount of observed data to include in the likelihood on
                 the model below. Click the Update Model button after you've made your selections."),
               fluidRow(
                   column(3),
                   column(6, 
                          actionButton("updateButton", "Update Model")
                          ),
               ),
               p(),
               p("
                    \\(
                        \\begin{align*}
                            Strontium.Ratio_i &\\sim N(\\mu_i, \\sigma^2) \\\\
                            \\mu_i &= \\alpha + \\sum_k w_k * g_k(Age_i) \\\\
                            \\alpha &\\sim N(\\mu_{\\alpha}, \\sigma_{\\alpha}^2) \\\\
                            w_j &\\sim N(\\mu_w, \\sigma_w^2) \\\\
                            \\sigma &\\sim Exp(\\lambda)
                        \\end{align*}
                    \\)
                "),
               fluidRow(
                   column(6,
                          # Inputs for Mean
                          sliderInput("mu", "\\(\\mu_\\alpha\\)", min=-5, max=5, value=0)
                   ),
                   column(6,
                          sliderInput("sigma", "\\(\\sigma_\\alpha\\)", min=0.1, max=5, value=1)
                   )
               ),
               fluidRow(
                   column(6,
                          sliderInput("lambda", "\\(\\lambda\\)", min=0.1, max=5, value=1)
                    ),
                   column(6,
                          sliderInput("k", "k", min=3, max=10, value=7)
                   )
               )
        ),
        
        column(6,
               fluidRow(plotOutput("dataPlot", height="400px")),
               # Inputs for % Data to Use in Likelihood
               fluidRow(
                   column(3),
                   column(6, 
                          sliderInput("percData", "% Data to Include", min=10, max=100, value=100),
                    )
                )
        )
    ),
    
    fluidRow(
             tabsetPanel(
                 tabPanel("1. Prior Predictive Distribution",
                            fluidRow(column(12,
                                            plotOutput("priorPlot", height="300px"))
                            )
                          ),
                 tabPanel("2. Diagnostic Plots"
                          ),
                 tabPanel("3. Posterior Distribution"
                          ),
                 tabPanel("4. Posterior Predictive Distribution"
                          )
             )
        )
))

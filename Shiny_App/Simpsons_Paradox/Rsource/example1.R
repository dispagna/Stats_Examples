require(dagitty)
require(visNetwork)

source("./Shiny_App/Simpsons_Paradox/Rsource/helperFunctions.R")

simpsonsEx1 <- function(covariate)
{

  df <- createDF(eval(parse(text = "c(\"insured\", \"uninsured\")")))

  allORs(df)
  
  # DAG1
  
  g1 <- dagitty("dag {
              drug <- insurance -> recover
              drug -> recover
              }")
  plot(graphLayout(g1))
  impliedConditionalIndependencies(g1)
  
  # visNetwork
  nodes <- data.frame(id = 1:3,
                      label = c("drug", "insured", "recover"),
                      shape = "circle",
                      color = "lightblue")
  
  edges <- data.frame(from = c(1, 2, 2), to = c(3, 1, 3),
                      color = c("darkblue", "red", "red"),
                      arrows="to")
  
  visNetwork(nodes,edges) %>%
    visLayout(randomSeed = 12)
}

require(dagitty)
require(visNetwork)

source("./Shiny_App/Simpsons_Paradox/Rsource/helperFunctions.R")

simpsonsEx2 <- function()
{
  
  df <- createDF(eval(parse(text = "c(\"unpaid\", \"paid\")")))
  
  allORs(df)
  
  # DAG1
  
  g1 <- dagitty("dag {
              drug -> paid <- recover
              drug -> recover
              }")
  plot(graphLayout(g1))
  impliedConditionalIndependencies(g1)
  
  # visNetwork
  nodes <- data.frame(id = 1:3,
                      label = c("drug", "paid", "recover"),
                      shape = "circle",
                      color = "lightblue")
  
  edges <- data.frame(from = c(1, 1, 3), to = c(3, 2, 2),
                      color = c("darkblue", "red", "red"),
                      arrows="to")
  
  visNetwork(nodes,edges) %>%
    visLayout(randomSeed = 12)
}


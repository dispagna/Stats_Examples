require(dagitty)
require(visNetwork)

source("./Rsource/helperFunctions.R")

simpsonsEx2 <- function()
{
  
  df <- createDF(eval(parse(text = "c(\"Unpaid\", \"Paid\")")))
  
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
                      label = c("Drug", "Paid", "Recovered"),
                      shape = "ellipse",
                      color = "lightblue")
  
  edges <- data.frame(from = c(1, 1, 3), to = c(3, 2, 2),
                      color = c("darkblue", "red", "red"),
                      arrows="to")
  
  vis <- visNetwork(nodes,edges) %>%
    visLayout(randomSeed = 12)
  
  list(tbl = data.frame(df),
       vis = vis)
}


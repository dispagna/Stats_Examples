require(dagitty)
require(visNetwork)

source("./Rsource/helperFunctions.R")

simpsonsEx1 <- function()
{

  df <- createDF(eval(parse(text = "c(\"Insured\", \"Uninsured\")")))

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
                      label = c("Drug", "Insured", "Recovered"),
                      shape = "ellipse",
                      color = "lightblue")
  
  edges <- data.frame(from = c(1, 2, 2), to = c(3, 1, 3),
                      color = c("darkblue", "red", "red"),
                      arrows="to")
  
  vis <- visNetwork(nodes,edges) %>%
    visLayout(randomSeed = 12)
  
  tmp <- data.frame(df)
  colnames(tmp) = c("Outcome", "Var1", "Var2", "Freq")
  
  list(tbl = tmp,
       vis = vis)
}

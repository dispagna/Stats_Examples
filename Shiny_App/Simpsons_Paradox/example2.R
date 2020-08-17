library(dagitty)

source("./Shiny_App/Simpsons_Paradox/helperFunctions.R")

df <- createDF(eval(parse(text = "c(\"unpaid\", \"paid\")")))

allORs(df)

# DAG1

g1 <- dagitty("dag {
              drug -> paid <- recover
              drug -> recover
              }")
plot(graphLayout(g1))
impliedConditionalIndependencies(g1)


library(dagitty)

df <- as.table(array(c(192, 71, 55, 25, 81, 6, 234, 36), dim=c(2,2,2),
                          dimnames=list(c("recovered", "died"),
                                        c("Drug", "No Drug"),
                                        c("F", "M"))))
allORs(df)

# DAG1

g1 <- dagitty("dag {
              gender -> recover <- drug
              }")
plot(graphLayout(g1))
impliedConditionalIndependencies(g1)

# DAG2

g2 <- dagitty("dag {
              gender -> recover <- drug
              gender -> drug
              }")
plot(graphLayout(g2))
impliedConditionalIndependencies(g2)

# DAG3

g3 <- dagitty("dag {
               drug <- gender -> recover
              }")
plot(graphLayout(g3))
impliedConditionalIndependencies(g3)

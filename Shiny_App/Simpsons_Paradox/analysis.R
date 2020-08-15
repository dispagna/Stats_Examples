library(dagitty)

oddsratio <- function(x)
{
  return((x[1,1] * x[2,2])/(x[2,1]*x[1,2]))
}

allORs <- function(x)
{
  marginalOR <- oddsratio(margin.table(x,c(1,2)))
  condORs <- apply(x, 3, oddsratio)
  return(append(marginalOR, condORs))
}

# First school
school1 <- as.table(array(c(60, 900, 45, 540, 90, 300, 30, 90), dim=c(2,2,2),
                          dimnames=list(c("F", "M"),
                                        c("admit", "reject"),
                                        c("A", "B"))))
allORs(school1)

# Second school
school2 <- as.table(array(c(150, 600, 65, 310, 350, 600, 60, 320), dim=c(2,2,2),
                          dimnames=list(c("F", "M"),
                                        c("admit", "reject"),
                                        c("A", "B"))))
allORs(school2)

tbl <- school2

# Initial DAG
g1 <- dagitty("dag {
              gender -> admit <- dept
              }")
plot(graphLayout(g1))
print(impliedConditionalIndependencies(g1))

# Gender ~ Admit
genderAdmit <- margin.table(tbl, c(1,2))
chisq.test(genderAdmit)

# Gender ~ Dept
genderDept <- margin.table(tbl, c(1,3))
chisq.test(genderDept)

# Dept ~ Admit
deptAdmit <- margin.table(tbl, c(2,3))
chisq.test(deptAdmit)

# Gender ~ Admit | Dept
chisq.test(tbl[,,1])
chisq.test(tbl[,,2])

# 1st Modified DAG
g2 <- dagitty("dag {
              gender -> admit <- dept
              gender -> dept
              }")
plot(graphLayout(g2))
print(impliedConditionalIndependencies(g2))

# 2nd Modified DAG
g3 <- dagitty("dag {
              gender -> dept -> admit
              }")
plot(graphLayout(g3))
print(impliedConditionalIndependencies(g3))





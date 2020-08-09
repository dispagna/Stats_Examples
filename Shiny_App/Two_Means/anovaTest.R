source("lrTest.R")

anovaResults <- function(samplesX, samplesY)
{
  anova(lrResults(samplesX, samplesY))
}
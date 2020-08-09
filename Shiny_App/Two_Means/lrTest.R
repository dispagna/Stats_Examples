library(tidyverse)

bindSamples <- function(samplesX, samplesY)
{
  df <- rbind(data.frame(label = "X",
                         values = samplesX,
                         idx = 0),
              data.frame(label = "Y",
                         values = samplesY,
                         idx = 1)) 
}

lrPlot <- function(samplesX, samplesY)
{
  
  bindSamples(samplesX, samplesY) %>%
    ggplot(aes(x=idx, y=values, colour=label, group=1)) +
    geom_point() +
    geom_smooth(method="lm")
}

lrResults <- function(samplesX, samplesY)
{
  df <- bindSamples(samplesX, samplesY)
  
  summary(lm(values ~ idx, data=df))
}
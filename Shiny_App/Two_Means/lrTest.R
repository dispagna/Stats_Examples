library(tidyverse)

bindSamples <- function(samplesX, samplesY)
{
  df <- rbind(data.frame(idx = "X",
                         values = samplesX),
              data.frame(idx = "Y",
                         values = samplesY)) 
}

lrPlot <- function(samplesX, samplesY)
{
  
  bindSamples(samplesX, samplesY) %>%
    ggplot(aes(x=idx, y=values, colour=idx, group=1)) +
    geom_point() +
    geom_smooth(method="lm") +
    theme(legend.title = element_blank()) +
    labs(x="Group")
}

lrResults <- function(samplesX, samplesY)
{
  df <- bindSamples(samplesX, samplesY)
  
  lm(values ~ idx, data=df)
}


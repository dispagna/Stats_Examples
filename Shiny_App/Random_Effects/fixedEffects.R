require(dplyr)
source("./bindSamples.R")

plotFixed <- function(meanX, sdX, samplesX,
                         meanY, sdY, samplesY,
                         meanZ, sdZ, samplesZ)
{
  dfX <- data.frame(lbl = "Cheerios",
                    idx = seq(meanX - 3*sdX, 
                              meanX + 3*sdX, 
                              0.1)) %>%
    mutate(prob = dnorm(idx, meanX, sdX))
  
  dfY <- data.frame(lbl = "Froot Loops",
                    idx = seq(meanY - 3*sdY, 
                              meanY + 3*sdY, 
                              0.1)) %>%
    mutate(prob = dnorm(idx, meanY, sdY))
  
  dfZ <- data.frame(lbl = "Raisin Bran",
                    idx = seq(meanZ - 3*sdZ, 
                              meanZ + 3*sdZ, 
                              0.1)) %>%
    mutate(prob = dnorm(idx, meanZ, sdZ))
  
  rbind(dfX, dfY, dfZ) %>%
    ggplot(aes(colour=lbl)) +
    geom_line(aes(x=idx, y=prob), linetype="dashed") +
    geom_point(data.frame(lbl="Cheerios", x=samplesX), 
               mapping=aes(x=x, y=0)) +
    geom_point(data.frame(lbl="Froot Loops", x=samplesY), 
               mapping=aes(x=x, y=0)) +
    geom_point(data.frame(lbl="Raisin Bran", x=samplesZ), 
               mapping=aes(x=x, y=0)) +
    theme(legend.title = element_blank()) +
    labs(x = "Sugar (g)", y="Probability")
}


anovaFixed <- function(samplesX, samplesY, samplesZ)
{
  df <- bindSamples(samplesX, samplesY, samplesZ)
  
  aov(values ~ Brand, data=df)

}


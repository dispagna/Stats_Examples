
plotTwoNorms <- function(meanX, sdX, samplesX,
                         meanY, sdY, samplesY)
{
  dfX <- data.frame(lbl = "X",
                    idx = seq(meanX - 3*sdX, 
                              meanX + 3*sdX, 
                              0.1)) %>%
    mutate(prob = dnorm(idx, meanX, sdX))
  
  dfY <- data.frame(lbl = "Y",
                    idx = seq(meanY - 3*sdY, 
                              meanY + 3*sdY, 
                              0.1)) %>%
    mutate(prob = dnorm(idx, meanY, sdY))
  
  rbind(dfX, dfY) %>%
    ggplot(aes(colour=lbl)) +
    geom_line(aes(x=idx, y=prob)) +
    geom_point(data.frame(lbl="X", x=samplesX), 
               mapping=aes(x=x, y=0)) +
    geom_point(data.frame(lbl="Y", x=samplesY), 
               mapping=aes(x=x, y=0)) +
    theme(legend.title=element_blank())
}
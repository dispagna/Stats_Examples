library(BEST)
library(tidyverse)
library(gridExtra)

bayesPlots <- function(samplesX, samplesY)
{
  priors <- BESTmcmc(samplesX, samplesY, doPriorsOnly = TRUE)
  posteriors <- BESTmcmc(samplesX, samplesY)
  
  priorMeans <- priors %>%
    select(c(mu1, mu2)) %>%
    rename(X=mu1, Y=mu2) %>%
    pivot_longer(cols=c(X,Y), names_to = "population", values_to = "val") %>%
    mutate(param = "Mean", dist = "prior")
  
  postMeans <- posteriors %>%
    select(c(mu1, mu2)) %>%
    rename(X=mu1, Y=mu2) %>%
    pivot_longer(cols=c(X,Y), names_to = "population", values_to = "val") %>%
    mutate(param = "Mean", dist="posterior")
  
  plotMeans <- rbind(priorMeans, postMeans) %>%
    ggplot(mapping=aes(val, colour=dist, fill=dist)) +
    geom_density(alpha=0.1) +
    facet_grid( ~ population) +
    labs(title="Mean")
    
  priorSd <- priors %>%
    select(c(sigma1, sigma2)) %>%
    rename(X=sigma1, Y=sigma2) %>%
    pivot_longer(cols=c(X,Y), names_to = "population", values_to = "val") %>%
    mutate(param = "Std. Dev.", dist="prior")
  
  postSd <- posteriors %>%
    select(c(sigma1, sigma2)) %>%
    rename(X=sigma1, Y=sigma2) %>%
    pivot_longer(cols=c(X,Y), names_to = "population", values_to = "val") %>%
    mutate(param = "Std. Dev.", dist="posterior")
  
  plotSDs <- rbind(priorSd, postSd) %>%
    ggplot(mapping=aes(val, colour=dist, fill=dist)) +
    geom_density(alpha=0.1) +
    facet_grid( ~ population) +
    labs(title="Standard Deviation")
  
  grid.arrange(plotMeans, plotSDs, ncol=1)
  
}


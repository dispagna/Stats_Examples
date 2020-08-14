library(rethinking)
library(tidyverse)
library(gridExtra)

samplesX <- rnorm(10)
samplesY <- rnorm(10,1)

quapModels <- function(samplesX, samplesY)
{

  # Model with non-informative priors
  flist <- alist(
    val ~ dnorm(mu, sigma),
    mu ~ dnorm(0, 20),
    sigma ~ dunif(0, 20)
  )
  
  # Quadratic Approximation
  mdl.X <- quap(flist, data=data.frame(val=samplesX), start=list(mu=0, sigma=5))
  mdl.Y <- quap(flist, data=data.frame(val=samplesY), start=list(mu=0, sigma=5))

  N <- 1e4
  
  # Prior samples
  dfPrior <- rbind(data.frame(extract.prior(mdl.X, N)) %>%
    mutate(population = "X"),
    data.frame(extract.prior(mdl.Y, N)) %>%
      mutate(population = "Y")) %>%
    mutate(type="prior")
  
  # Posterior samples
  dfPost <- rbind(data.frame(extract.samples(mdl.X, N)) %>%
                     mutate(population = "X"),
                   data.frame(extract.samples(mdl.Y, N)) %>%
                     mutate(population = "Y")) %>%
    mutate(type="posterior")
  
  meansPlt <- rbind(dfPrior, dfPost) %>%
    ggplot(mapping=aes(mu, colour=type, fill=type, linetype=type)) +
    geom_density(alpha=0.1) +
    facet_grid(~population) +
    xlim(c(-10, 10)) +
    labs(x = "Individual Population Means")
  
  # Now get difference of means
  dfDiff <- rbind(data.frame(type="posterior", 
                             diff=dfPost$mu[1:N] - dfPost$mu[(N+1):(2*N)]))
  
  credible <- HPDI(dfDiff %>% filter(type=="posterior") %>% select(diff),
                   prob=0.95)
  
  diffPlt <- dfDiff %>%
    ggplot(mapping=aes(diff, colour=type, fill=type, linetype=type)) +
    geom_density(alpha=0.1) +
    geom_vline(xintercept=credible, color="blue", linetype="dashed", 
               show.legend = TRUE) +
    xlim(c(-10, 10)) +
    labs(x = "Difference Between Population Means")
  
  list(meansPlt = meansPlt,
       diffPlt = diffPlt,
       credible = credible)
    
}



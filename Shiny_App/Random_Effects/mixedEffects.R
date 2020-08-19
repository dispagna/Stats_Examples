require(lme4)

source("./bindSamples.R")

plotRandom <- function(samplesX, samplesY, samplesZ,
                       mu, totalVar)
{
  df <- data.frame(idx = seq(mu-3*sqrt(totalVar), mu+3*sqrt(totalVar), 0.1)) %>%
    mutate(prob = dnorm(idx, mu, sqrt(totalVar)),
           lbl = "All Brands")
  
  
  ggplot(mapping=aes(colour=lbl)) +
    geom_point(data.frame(lbl="Cheerios", x=samplesX), 
               mapping=aes(x=x, y=0)) +
    geom_point(data.frame(lbl="Froot Loops", x=samplesY), 
               mapping=aes(x=x, y=0)) +
    geom_point(data.frame(lbl="Raisin Bran", x=samplesZ), 
               mapping=aes(x=x, y=0)) +
    geom_line(data=df, mapping=aes(x=idx, y=prob), linetype="dashed") +
    theme(legend.title=element_blank()) +
    labs(x = "Sugar (g)", y="Probability")
  
}

anovaRandom <- function(samplesX, samplesY, samplesZ)
{
  df <- bindSamples(samplesX, samplesY, samplesZ)
  
  mdl <- lmer(values ~(1 |Brand), data=df)
  overallMean <- fixef(mdl)
  overallVar <- sum(as.data.frame(VarCorr(mdl)) %>%
                    select(vcov))
  
  return(list(mdl = mdl,
              overallMean = overallMean,
              overallVar = overallVar))
}
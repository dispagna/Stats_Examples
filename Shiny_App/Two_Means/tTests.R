library(broom)

tTests <- function(samplesX, samplesY)
{
  # Pooled t-Test
  pooled <- tidy(t.test(samplesX, samplesY, var.equal = TRUE)) %>%
    select(method, conf.low, conf.high, statistic, p.value)
  
  # Welch's t-Test
  welch <- tidy(t.test(samplesX, samplesY)) %>%
    select(method, conf.low, conf.high, statistic, p.value)
  
  rbind(pooled, welch)
}
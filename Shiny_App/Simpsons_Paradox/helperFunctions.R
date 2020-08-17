
createDF <- function(covariate)
{
  df <- as.table(array(c(1920, 710, 550, 250, 810, 60, 2340, 360), dim=c(2,2,2),
                       dimnames=list(c("recovered", "died"),
                                     c("Drug", "No Drug"),
                                     covariate)))
  return(df)
}

oddsratio <- function(x, prob=0.95)
{
  alpha <- 1-prob
  OR <- (x[1,1]*x[2,2])/ (x[2,1]*x[1,2])
  seLogOR <- sqrt(1/x[1,1] + 1/x[1,2] + 1/x[2,1] + 1/x[2,2])
  lowerOR <- OR*exp(qnorm(alpha/2) * seLogOR)
  upperOR <- OR*exp(-qnorm(alpha/2) * seLogOR)
  res <- c(OR, lowerOR, upperOR)
  names(res) <- c("OR", "lwr", "upr")
  return(res)
  
}

allORs <- function(x)
{
  marginal <- oddsratio(margin.table(x,c(1,2)))
  condORs <- apply(x, 3, oddsratio)
  return(rbind(marginal, t(condORs)))
}

testMarginalIndependence <- function(x)
{
  # Test for unconditional independence between all combinations of variables
  pairs <- list("c(1,2)", "c(1,3)", "c(2,3)")
  
  res <- list()
  
  for (i in 1:length(pairs))
  {
    # Get marginal table
    tmp <- margin.table(x, eval(parse(text=pairs[[i]])))
    dims <- dimnames(tmp)
    if (chisq.test(tmp)$p.value > 0.1)
    {
      res <- append(res, paste(paste(dims[[1]], collapse="/"), " _| |_ ", 
                        paste(dims[[2]], collapse="/")))
    }
  }
  
  return(res)
}

testConditionalIndependence <- function(x)
{
  res <- list()
  
  dims <- 1:length(dim(x))
  
  for(i in dims)
  {
    tmp <- apply(x, i, chisq.test)
    for (j in 1:length(tmp))
    {
      if (tmp[[j]]$p.value > 0.1)
      {
        cond <- names(tmp[j])
        marg <- setdiff(dims, i)
        res <- append(res, paste(paste(dimnames(x)[[marg[1]]], collapse="/"), " _| |_ ", 
                          paste(dimnames(x)[[marg[2]]], collapse="/"), "  | ", 
                          cond))
      }
    }
  }
  
  return(res)
}
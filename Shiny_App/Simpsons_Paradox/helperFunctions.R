

oddsratio <- function(x)
{
  OR <- (x[1,1]*x[2,2])/ (x[2,1]*x[1,2])
  seLogOR <- sqrt(1/x[1,1] + 1/x[1,2] + 1/x[2,1] + 1/x[2,2])
  lowerOR <- OR*exp(-1.96 * seLogOR)
  upperOR <- OR*exp(1.96 * seLogOR)
  return(list(OR = OR,
         lwr = lowerOR,
         upr = upperOR))
  
}

allORs <- function(x)
{
  marginalOR <- oddsratio(margin.table(x,c(1,2)))
  condORs <- apply(x, 3, oddsratio)
  return(append(marginalOR, condORs))
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
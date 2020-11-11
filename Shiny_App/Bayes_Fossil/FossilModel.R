library(tidyverse)
library(rstanarm)
library(bayesplot)
library(parallel)

#' Reads in fossil data
#' 
#' @param percData Percentage of data to include in model
#' 
#' @return fossil data frame
#' 
getFossilData <- function(percData)
{
  fossil <- read.csv("./fossil.csv") %>%
    mutate(include=FALSE)
  
  N <- nrow(fossil)
  
  fossil$include[sample(1:N, floor(N*percData))] = TRUE
  fossil$include <- factor(fossil$include, levels = c("TRUE", "FALSE"))
  
  fossil
}

#' Plots the fossil data
#' 
#' @param fossil data frame with fossil data
#' 
#' @return ggplot object
#' 
plotFossilData <- function(fossil)
{
  fossil %>%
    ggplot() +
    geom_point(mapping=aes(x=Age, y=Strontium.Ratio, color=include,
                           alpha=include)) +
    scale_alpha_discrete(range=c(1.0, 0.3))
}

#' Fit model both post and prior
#' 
#' @param fossil data frame with fossil data
#' @param mu     prior mean for intercept
#' @param sigma  prior std deviation for intercept
#' @param lambda prior mean for variance
#' @param k      dimension of basis (i.e., number of spline terms)
#' @param std    boolean indicating whether or not to standardize data
#'               before fitting model
getModel <- function(fossil, mu, sigma, lambda, k, std=TRUE)
{
  ncores <- min(max(1,detectCores()-1), 4)
  
  fossil_inc <- fossil %>%
    filter(include==TRUE) 
  
  if (std)
  {
  fossil_inc <- fossil_inc %>%
    mutate(across(where(is.numeric), scale))
  }

  post <- stan_gamm4(Strontium.Ratio ~ s(Age, bs="cr", k=k), 
                    data = fossil_inc, 
                    refresh=-1,
                    cores=ncores,
                    adapt_delta = ifelse(std, 0.99, 0.95))
  
  prior <- suppressWarnings(update(
    post,
    prior_PD = TRUE,
    refresh = -1,
    chains = 2,
    cores=ncores
  ))
  
  list(prior=prior, post=post)
}

#' Plots predictions from model
#' 
#' @ param mdl stanreg object
plotPred <- function(mdl)
{
  N <- 50

  D <- seq(min(mdl$data$Age), max(mdl$data$Age), length.out = N)
  
  pred <- data.frame(t(posterior_epred(mdl,
                                             newdata=data.frame(Age=D),
                                             draws=N)))
  
  tmp <- pred %>%
    mutate(Age = D)%>%
    pivot_longer(cols=-"Age", names_to="iter", values_to="Strontium.Ratio") 
  
  tmp %>%
    ggplot(aes(x=Age, y=Strontium.Ratio)) +
    geom_line(aes(group=iter), alpha=0.2) +
    geom_point(data=mdl$data, aes(color=include, alpha=include)) +
    scale_alpha_discrete(range=c(1.0, 0.3))
}

#' Plot chain diagnostics
#'
#' @param mdl stanreg object
plotChains <- function(mdl)
{
  post <- as.array(mdl)
  mcmc_trace(post)
}

#' Prints model summary
#'
#' @param mdl stanreg object
getSummary <- function(mdl)
{
  if (!is.null(mdl))
  {
    summary(mdl, digits=5)
  }
}


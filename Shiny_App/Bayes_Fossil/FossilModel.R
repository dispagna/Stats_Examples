library(ggplot2)
library(dplyr)
library(rstanarm)
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

#' Prior predictive distribution
#' 
#' @param 
getPriorPred <- function(fossil, mu, sigma, lambda, k)
{
  ncores <- min(max(1,detectCores()-1), 4)
  
  fossil_inc <- fossil %>%
    filter(include==TRUE)
  
  mdl <- stan_gamm4(Strontium.Ratio ~ s(Age, bs="cr", k=k), 
                                data = fossil_inc, 
                                cores=ncores, 
                                prior_PD = TRUE)
  mdl
}

#' Prior predictive distribution
#' 
#' @param 
getPostPred <- function(fossil, mu, sigma, lambda, k)
{
  ncores <- min(max(1,detectCores()-1), 4)
  
  fossil_inc <- fossil %>%
    filter(include==TRUE)
  
  mdl <- stan_gamm4(Strontium.Ratio ~ s(Age, bs="cr", k=6), 
                    data = fossil_inc, 
                    cores=ncores)
  mdl
}

plotPred <- function(mdl, fossil)
{
  N <- 50
  
  D <- seq(min(fossil$Age), max(fossil$Age), length.out = N)
  
  pred <- data.frame(t(posterior_epred(mdl,
                                             newdata=data.frame(Age=D),
                                             draws=N)))
  
  tmp <- pred %>%
    mutate(Age = D)%>%
    pivot_longer(cols=-"Age", names_to="iter", values_to="Strontium.Ratio") 
  
  tmp %>%
    ggplot(aes(x=Age, y=Strontium.Ratio)) +
    geom_line(aes(group=iter), alpha=0.2) +
    geom_point(data=fossil, aes(color=include, alpha=include)) +
    scale_alpha_discrete(range=c(1.0, 0.3))
}

#'
#'
#'
plotChains <- function(mdl)
{
  post <- as.array(mdl)
  mcmc_trace(post)
}

#'
#'
#'
getSummary <- function(mdl)
{
  if (!is.null(mdl))
  {
    summary(mdl)
  }
}

#'
#'
#'
plotPost <- function(mdl, fossil)
{
  plot_nonlinear(mdl, prob=0.89) +
    geom_point(data=fossil, mapping=aes(x=Age, y=Strontium.Ratio-mean(Strontium.Ratio), 
                                        color=include, alpha=include)) +
    scale_alpha_discrete(range=c(1.0, 0.3))
}
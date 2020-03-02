random <- function(payoff, ntrials, b) {

  #theta <- .7
  #b <- c(theta,1-theta)
  
  x <- array(0,c(ntrials))
  r <- array(0,c(ntrials))
  
  for (t in 1:ntrials) {
    #agent that chooses randomly between options 1 and 2, with bias (to 1) = theta
    x[t] <- rcat(1,b)
    
    #what reward does the agent get?
    r[t] <- payoff[t, x[t]]
  }
  result <- list(x=x, r=r)
  return(result)
}
pacman::p_load(devtools, digest)
options(repos = getOption("repos")["CRAN"])
devtools::install_github("ludvigolsen/cvms", ref="hparams_tuning")
10

library(cvms)

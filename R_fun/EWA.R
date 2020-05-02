EWA <- function(n_agents=100, n_trials=20, n_tokens=20, pi, delta, rho, phi, lambda){
  
  # setup
  c <- array(0, c(n_agents, n_trials)) #actual contribution
  n <- array(0, c(n_agents, n_trials)) #experience counter, later weighted
  A <- array(0, c(n_agents, n_trials, n_tokens)) #attraction
  p <- array(0, c(n_agents, n_trials, n_tokens)) #probability of choice
  
  n[, 1] <- 1
  c[,1] <- 20
  A[,1,] <- 0
  
  for (t in 2:n_trials){
    for (a in 1:n_agents){
      n[a, t] <- (rho[a]*n[a, t-1]) + 1  # eq. 2.1 (experience updating)
      
      for (tok in 1:n_tokens){
        A[a, t, tok] <- (
          (phi[a]*n[a, t-1]*A[a, t-1, tok]) +  # prior attraction
            (delta[a] + ((1-delta[a])*(c[a, t-1] == tok))) *  # indicates whether the token was chosen, regret-weighted payoff
            ((((tok + sum(c[-a, t-1]))*pi)/n_agents)-tok)  # payoff for each possible contrib.
        ) / n[a, t]  # experience weighting
      }
      p[a, t, ] <- exp(lambda[a]*A[a, t,])/sum(exp(lambda[a]*A[a, t, ]))  # softmax
      c[a, t] <- extraDistr::rcat(1, p[a, t, ])
    }
  }
  
  internal_states=list(delta=delta, rho=rho, phi=phi, lambda=lambda)
  res <- list(choice=c, n=n, prob=p, pi=pi, internal_states=internal_states)
  return(res)
}
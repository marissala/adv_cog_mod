ORL <- function(payoff,ntrials,a_rew, a_pun, beta_f, beta_p, K, theta) {
  
  # Arrays to populate
  x <- array(0, c(ntrials)) #choice
  r <- array(0, c(ntrials)) #reward
  Ev <- array(0, c(ntrials,4)) #expected value
  Ev_update <- array(0, c(ntrials,4))
  Ef <- array(0, c(ntrials,4)) #expected frequency
  Ef_chosen <- array(0, c(ntrials,4))
  Ef_not <- array(0, c(ntrials,4))
  
  #perseverance variable
  PS <- array(0, c(ntrials, 4))
  #valence of each deck - linear combo of expected valence, expected frequency and perseverance
  V <- array(0, c(ntrials, 4))
  
  exp_p <- array(0, c(ntrials,4)) #exponentialized part of the prob value
  p <- array(0, c(ntrials,4)) #need a p for each deck
  
  # Initiate x with equal chance to draw 1st card from any deck
  x[1] <- rcat(1, c(.25,.25,.25,.25))
  
  # Building the model
  for (t in 2:ntrials) {
    
    # New variable that tracks the sign
    signX <- ifelse(r[t-1]<0,-1,1)
    
    for (d in 1:4) {
      
      # Ev_update now uses a different learning rate for wins and losses
      Ev_update[t,d] <- ifelse(r[t] > 0, Ev[t-1,d] + (a_rew * (r[t] - Ev[t-1,d])), Ev[t-1,d] + (a_pun * (r[t] - Ev[t-1,d])))
      
      # This remains the same as in PVL-delta
      Ev[t,d] <- ifelse(x[t-1] == d, Ev_update[t,d], Ev[t-1,d])
      #if we are on the same deck as we are updating then update ev, otherwise keep last value?
      
      # Frequency, use signX instead of magnitude, also need separate to rewards and losses
      Ef_chosen[t,d] <- ifelse(signX > 0, Ef[t-1,d] + (a_rew * (signX(r[t]) - Ef[t-1,d])), Ef[t-1,d] + (a_pun * (signX(r[t]) - Ef[t-1,d])))
      
      Ef_not[t,d] <- ifelse(signX < 0, Ef[t-1,d] + (a_pun * ( (-signX(r[t]))/C - Ef[t-1,d])), Ef[t-1,d] + (a_rew * ((-signX(r[t]))/C - Ef[t-1,d])))
      
      # Ef updating is similar to Ev updating
      Ef[t,d] <- ifelse(x[t-1] == d, Ef_chosen[t,d], Ef_not[t,d])
      
      # Perseverance
      PS[t,d] <- ifelse(x[t-1] == d, 1/(1 + K), PS[t-1,d]/(1+K))
      
      # Linear combination V variable calculations, same as linear regression
      V[t,d] <- Ev[t,d] + Ef[t,d] * beta_f + PS[t,d] * beta_p
      
      # Calculate the softmax
      exp_p[t,d] <- exp(theta * V[t,d]) # with V? or Ev or Ef?
      #softmax
      
    }
    
    for (d in 1:4) {
      p[t,d] <- exp_p[t,d] / sum(exp_p[t,])
    }
    
    x[t] <- rcat(1,p[t,]) #sample
    
    r[t] <- payoff[t,x[t]] #value
    
  }
  
  result <- list(x=x, r=r, Ev=Ev, Ef=Ef, PS=PS, V=V)
  return(result)
  
}

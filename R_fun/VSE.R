VSE <- function(L,R, ntrials, r_pref, delta_rate, a_explore, phi, theta) {
  
  # Arrays to populate
  x <- array(0, c(ntrials)) #choice
  r <- array(0, c(ntrials)) #reward
  l <- array(0, c(ntrials)) #losses
  v <- array(0, c(ntrials,4)) #utility
  
  # Exploit variable
  exploit <- array(0, c(ntrials,4))
  explore <- array(0, c(ntrials,4))
  
  exp_p <- array(0, c(ntrials,4)) #exponentialized part of the prob value
  p <- array(0, c(ntrials,4)) #need a p for each deck
  
  # Initiate x with equal chance to draw 1st card from any deck
  x[1] <- rcat(1, c(.25,.25,.25,.25))
  r[1] <- R[1, x[1]]
  l[1] <- L[1, x[1]]
  
  # Building the model
  for (t in 2:ntrials) {
    
    # Value function
    v[t-1] <- R[t-1]^r_pref - L[t-1]^r_pref
    
    for (d in 1:4) {
      
      # Start with exploit variable for chosen and unchosen decks (decay rule)
      exploit[t,d] <- ifelse(x[t-1] == d, exploit[t-1,d]*delta_rate + v[t-1], exploit[t-1,d]*delta_rate)
      
      # Explore variable for chosen and unchosen decks
      explore[t,d] <- ifelse(x[t-1] == d, 0, explore[t-1,d] + a_explore * (phi - explore[t-1,d]))
      
      exp_p[t,d] <- exp(theta * (exploit[t,d] + explore[t,d]))
      #softmax
      
    }
    
    for (d in 1:4) {
      p[t,d] <- exp_p[t,d] / sum(exp_p[t,])
    }
    
    x[t] <- rcat(1,p[t,]) #sample
    
    #solve this for R and L
    r[t] <- R[t,x[t]] #value
    l[t] <- R[t,x[t]]
    
  }
  
  #change this
  result <- list(x=x, r=r, l=l, explore=explore, exploit=exploit)
  return(result)
  
}
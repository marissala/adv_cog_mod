VSE <- function(L,R, ntrials, a_rew, a_pun, beta_f, beta_p, K, theta) {
  
  # Arrays to populate
  x <- array(0, c(ntrials)) #choice
  r <- array(0, c(ntrials)) #reward
  v <- array(0, c(ntrials,4)) #utility
  
  # Exploit variable
  exploit <- array(0, c(ntrials,4))
  explore <- array(0, c(ntrials,4))
  
  exp_p <- array(0, c(ntrials,4)) #exponentialized part of the prob value
  p <- array(0, c(ntrials,4)) #need a p for each deck
  
  # Initiate x with equal chance to draw 1st card from any deck
  x[1] <- rcat(1, c(.25,.25,.25,.25))
  
  # Building the model
  for (t in 2:ntrials) {
    
    # NEEDS A VALUE FUNCTION HERE
    #v[t] <- gain(t)^theta - loss(t)^theta
    
    
    #u[t,d] <- ifelse(X[t-1] < 0, -w*abs(X[t-1])^A, abs(X[t-1])^A) #utility for deck d on trial t; discontinuous function so we do ifelse, looks one way for positive and the other for negative values
    #if rew less than 0 then take the abs value of the rew, put it to the power of A for curve * -w for loss aversion
    
    
    for (d in 1:4) {
      
      # Start with exploit variable for chosen and unchosen decks (decay rule)
      exploit[t,d] <- ifelse(x[t-1] == d, exploit[t-1,d]*delta_rate + v[t,d], exploit[t-1,d]*delta_rate)
      
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
    r[t] <- payoff[t,x[t]] #value
    
  }
  
  #change this
  result <- list(x=x, X=X, Ev=Ev)
  return(result)
  
}

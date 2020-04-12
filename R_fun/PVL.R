PVL <- function(payoff,ntrials,w,A,a,theta) {
  
  # Arrays to populate
  x <- array(0, c(ntrials)) #choice
  X <- array(0, c(ntrials)) #reward
  u <- array(0, c(ntrials,4)) #utility
  Ev <- array(0, c(ntrials,4)) #expected value
  Ev_update <- array(0, c(ntrials,4))
  exp_p <- array(0, c(ntrials,4)) #exponentialized part of the prob value
  p <- array(0, c(ntrials,4)) #need a p for each deck
  
  ## EXTRA ONE
  #abs_temp <- array(0, c(ntrials,4))
  
  # Initiate x with equal chance to draw 1st card from any deck
  x[1] <- rcat(1, c(.25,.25,.25,.25))
  #X[1] <- payoff[1, x[1]]
  
  # Building the model
  for (t in 2:ntrials) {
    
    for (d in 1:4) {
      # FIX: IN CASE THE ^A IS APPROACHING INFINITY
      #abs_temp[t,d] <- ifelse((is.infinite(abs(X[t-1])^A)),
      #                       exp(50),
      #                       abs(X[t-1])^A)
      #abs_temp[t,d]<-abs_temp[!is.na(abs_temp[t,d])]
      #abs_temp[t,d] <- abs(X[t-1])^A
      #print("before: ")
      #print(abs(X[t-1])^A)
      
      u[t,d] <- ifelse(X[t-1] < 0, -w*abs(X[t-1])^A, X[t-1]^A) #utility for deck d on trial t; discontinuous function so we do ifelse, looks one way for positive and the other for negative values
      #u[t,d] <- ifelse(r[t-1] < 0, -w*abs(r[t-1])^a, r[t-1]^a)
      
      #if rew less than 0 then take the abs value of the rew, put it to the power of A for curve * -w for loss aversion
      #print("after: ")
      #print(abs(X[t-1])^A)
      
      Ev_update[t,d] <- Ev[t-1,d] + (a * (u[t,d] - Ev[t-1,d])) #delta learning rule is learning rate a * prediction error
      #expected valence on prev trial + new info (learning rate * pred error)
      
      Ev[t,d] <- ifelse(x[t-1] == d, Ev_update[t,d], Ev[t-1,d])
      #if we are on the same deck as we are updating then update ev, otherwise keep last value?
      
      exp_p[t,d] <- exp(theta * Ev[t,d])
      # FIX: IN CASE THE ^A IS APPROACHING INFINITY
      #if(is.infinite(exp_p[t,d])){exp_p[t,d] = exp(100)}
      #softmax
      
      }
    
    #exp_p[t,d] <- ifelse(
    #  (is.infinite(exp_p[t,d])),
    #  exp(50),
    #  exp_p[t,d])
    
    for (d in 1:4) {
      p[t,d] <- exp_p[t,d] / sum(exp_p[t,])
    }
    
    x[t] <- rcat(1,p[t,]) #sample
    X[t] <- payoff[t,x[t]] #value
    
  }
  
  result <- list(x=x, X=X, Ev=Ev)
  return(result)
  
}
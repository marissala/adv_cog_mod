---
title: "Class 3"
author: "Maris Sala"
date: "2/17/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
#pacman::p_load(R2jags)
library(R2jags)
```

Build forward simulations of 2 models
```{r}
set.seed(1982) #reproducibility important here
runif(1,0,1)

setwd("~/Aarhus University/Advanced cog mod/progre")

## MODEL 1: Fixed Theta model ##
Gfixed <- array(0, c(100)) #100 elements of 0s
#array(0, c(2,100)) if theres 2 agents for example
#empty array we will fill up with a simple forward model

#need a fixed parameter
theta <- .7 #anything thats less than 0 more than 1 is not legit
#someone who is okay at this task

ntrials <- 100

for (t in 1:ntrials) {
  Gfixed[t] <- rbinom(1,1,theta) #each element in the array populated with consequences of the model
}

sum(Gfixed) #we'd want this to be 70 ideally bc theta is .7
```

<- because then u can copy paste to jags

rbinom() - binomial distribution, can sample randomly, can find y and x of the distribution etc, r means randomly sampled, nr of trials, iterations, then parameter for the model (theta here)


MODEL 2: Learning model
```{r}
Glearn <- array(0, c(100)) #guess for our learning model
theta <- array(0, c(100)) #theta is going to update every trial
# we need a starting theta!
alpha <- .05 #learning rate, can never be interpreted contextually free
theta1 <- .5

ntrials <- 100

#need a theta on trial 1, have to define the 1st trial, simulation runs from trial 2
theta[1] <- theta1
Glearn[1] <- rbinom(1,1,theta[1])

for (t in 2:ntrials) {
  
  theta[t] <- theta[t-1]^(1/(1+alpha)) #this is the learning rule/curve, focus on this next week
  Glearn[t] <- rbinom(1,1,theta[t])

}

sum(Glearn)
theta

plot(theta)
```

jags is not sequential, updates all the distributions simulatneouly, so the for loop would run differently, debugging is odd and different from r (cant do trial by trial), but can fix the R code and implement in jags if they are connected enough


Run inference for the fixed model using the fixed data
```{r}
G <- Gfixed

data <- list("G", "ntrials") #has to be a list in this case, look in the workspace for G
params <- c("theta") #what are we interested in keeping track of, theta as a distribution

samples <- jags(data, inits = NULL, params,
                model.file = "chick.txt",
                n.chains = 3, n.iter = 5000, n.burnin = 1000, n.thin = 1) #inits could be important when there are convergence issues

samples

x <- samples$BUGSoutput$sims.list$theta #guesses at theta

plot(x)

plot(density(x)) #this is the posterior
#^this is the inference basically, it should be 0.7 but its more around 0.6

```
stuff we get out is a distribution, list of samples from a posterior distribution
the more chains the more independence you have in results but also the more comp power it takes

Inference on the learning model
```{r}
G <- Glearn

data <- list("G", "ntrials") #has to be a list in this case, look in the workspace for G
params <- c("theta", "theta1", "alpha") #what are we interested in keeping track of, theta as a distribution

samples <- jags(data, inits = NULL, params,
                model.file = "chick_learning.txt",
                n.chains = 3, n.iter = 5000, n.burnin = 1000, n.thin = 1) #inits could be important when there are convergence issues

samples

x <- samples$BUGSoutput$sims.list$theta #guesses at theta

plot(x)

plot(density(x)) #this is the posterior
```


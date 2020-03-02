---
title: "Class6"
author: "Maris Sala"
date: "3/2/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
set.seed(1982)
pacman::p_load(R2jags, extraDistr)
setwd("~/Aarhus University/Advanced cog mod/progre")
```

Generate task environment
```{r}
ntrials <- 100

Aprob <- .3
Arew <- 2

Bprob <- .8
Brew <- 1

#generate a payoff matrix
payoff <- cbind(rbinom(ntrials, 1, Aprob) * Arew, rbinom(ntrials, 1, Bprob) * Brew) #why rbinom?

colSums(payoff)
payoff
```


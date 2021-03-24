
##install JAGS : http://mcmc-jags.sourceforge.net
##install rjags package :
# install.packages("rjags")
library(rjags)
library(tidyverse)



setwd("/Users/stephanievandenberg/SURFdrive/Werk/Onderwijs/statistiek/book/linear models/book")







model.file <- "model1"

p <- c(0.80, 0.20)

M <- rep(0, 1)

s <- rep(sqrt(0.14), 1)

#Identify data for JAGS analysis
bugsdata <- list(s = s, M = M, p = p, values = c(3.25, 3.26))     

MoE <- sqrt(s^2 / 4) * 3.18


# give some reasonable starting values
inits <- list(mu_cat = 1)   

# run the model for adaptation period (default = 1000)
jags <- jags.model(model.file, bugsdata, inits, n.chains = 1, quiet=FALSE)

#Define number of extra iterations for burn-in:
n.iter <- 5000
update(jags, n.iter) 

# run the mcmc for inference and store values of the important model parameters
#Output jags samples
#define number of iterations for inference
n.iter <- 50000
out <- jags.samples(jags, c("mu_cat"), n.iter)

# posterior samples 
mu_cat <- out$mu_cat[1,,1]
mu <- mu_cat
mu <- ifelse(mu_cat == 1, 3.25, 3.26)
plot(density(mu), main = "mu")

mu %>% enframe() %>% 
  group_by(value) %>% 
  count() %>% 
  mutate(prop = n/n.iter)


interval <- (mu > (M - MoE) & mu < (M + MoE))
table(interval)


# Bayesian credibility region 
# based on highest posterior density principle
HPD <- function(sample1, rel.int) {
  rel.int <- (1 - rel.int)/2 #calculate range outside of credibility region (both sides 2.5 in this case)
  lower <- round(length(sample1) * rel.int, 0) 
  upper <- round(length(sample1) * (1 - rel.int), 0)
  diff.int <- upper - lower
  HPDo <- sample1[order(sample1)][1:lower]
  HPDb <- sample1[order(sample1)][(diff.int + 1):upper]
  HPDI <- round(c(HPDo[order(HPDb - HPDo)[1]], HPDb[order(HPDb - HPDo)[
    1]]), 5)
  #CI <- round(c(sample1[order(sample1)][lower], sample1[order(sample1)][
  #  upper]), 3)
  return(HPDI)
}
HPD(p, 0.95)




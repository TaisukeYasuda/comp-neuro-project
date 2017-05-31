# Taisuke Yasuda
#
# This file contains unit tests for em_epsp_binomial.R. The tests are simply
# testing the code on simulated data from a matching model. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
source("./scripts/em_epsp_binomial.R")

EPSP.Binomial <- function(mu, sigma, p, N, n) {
  # Samples n data points from an EPSP model with parameters mu, sigma, p, and
  # N. 
  # 
  # Args:
  #   mu: The mean amplitude for each of the synaptic contacts.
  #   sigma: The variance for each of the synaptic contacts. 
  #   p: The release probability for each of the synaptic contacts.
  #   N: The number of synaptic contacts. 
  
  SingleTrial <- function(dummy) {
    Y = rbinom(N, 1, p)
    NormalIfRelease <- function(y) {
      if (y == 1) {
        r = -1
        while (r < 0) {
          r = rnorm(1, mu, sqrt(sigma))
        }
        return(r)
      } else {
        return(0)
      }
    }
    return(sum(apply(array(Y), 1, NormalIfRelease)))
  }
  X = rep(0, n)
  return(apply(array(X), 1, SingleTrial))
}

mu <- 0.3
sigma <- 0.2
p <- 0.2
N <- 5
n <- 100

x = EPSP.Binomial(mu, sigma, p, N, n)
theta = EMBinomial(x, N)
print(theta)
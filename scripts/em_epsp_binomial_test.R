# Taisuke Yasuda
#
# This file contains unit tests for em_epsp_binomial.R. The tests are simply
# testing the code on simulated data from a matching model. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
source("./scripts/em_epsp_binomial.R")
source("./scripts/epsp_binomial.R")

mu <- 0.3
sigma <- 0.2
p <- 0.2
N <- 5
n <- 100

x = EPSP.binomial(mu, sigma, p, N, n)
theta = EMBinomial(x, N)
print(theta)
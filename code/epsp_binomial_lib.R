# Taisuke Yasuda
#
# This file implements useful functions for statistical analysis of the
# postsynaptic potential distribution under the "binomial distribution" model. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
library(functional)
library(Rcpp)
source("./code/filenames.R")

SampleFailureRate <- function(x.data) {
  # Computes the sample failure rate of the sample. 
  # 
  # Args:
  #   x.data: The n data points.
  # 
  # Returns:
  #   r: The sample failure rate. 
  
  n = length(x.data)
  x.fail.freq <- as.data.frame(table(x.data))
  x.fail.index <- which(x.fail.freq["x.data"] == 0)
  x.fail.count <- 0
  if (length(x.fail.index) == 0 || is.na(x.fail.index)) {
    x.fail.index <- -1
    x.fail.count <- 0
  } else {
    x.fail.count <- x.fail.freq[x.fail.index,"Freq"]
  }
  return(x.fail.count / n)
}

MOME.Binomial <- function(x.data, N) {
  # Returns the method of moments estimate for the parameters of the model. 
  #
  # Args: 
  #   x.data: The n data points. 
  #   N: The number of assumed synaptic contact points. 
  #
  # Returns:
  #   theta: The method of moments estimate with estimates contained in
  #     theta$mu, theta$sigma, and theta$p. 
  
  theta = list()
  # Estimate p
  p.fail <- SampleFailureRate(x.data)
  theta$p <- 1 - p.fail^(1/N)
  # Useful constants
  a <- var(x.data) / (N * theta$p)
  b <- mean(x.data) / (N * theta$p)
  c <- 0.5 * log(a + b^2)
  d <- 2 * log(b)
  # Estimate mu and sigma
  theta$mu <- d - c
  theta$sigma <- 2*c - d
  return(theta)
}

MOME.SimpleBinomial <- function(x.data, N) {
  # Returns the method of moments estimate for the parameters of the simple 
  # binomial model. 
  #
  # Args: 
  #   x.data: The n data points. 
  #   N: The number of assumed synaptic contact points. 
  #
  # Returns:
  #   theta: The method of moments estimate with estimates contained in
  #     theta$mu, theta$sigma, and theta$p. 
  
  theta = list()
  # Estimate p
  p.fail <- SampleFailureRate(x.data)
  theta$p <- 1 - p.fail^(1/N)
  # Estimate mu and sigma
  theta$mu <- mean(x.data) / (N * theta$p)
  theta$sigma <- var(x.data) / (N * theta$p)
  return(theta)
}

MLE.SingleContact <- function(x.data) {
  # Returns the MLE estimate for the parameters of the model with N = 1.
  #
  # Args: 
  #   x.data: The n data points. 
  #
  # Returns:
  #   theta: The method of moments estimate with estimates contained in
  #     theta$mu, theta$sigma, and theta$p. 
  
  theta = list()
  # Estimate p
  p.fail <- SampleFailureRate(x.data)
  theta$p <- 1 - p.fail
  # Take log of nonzero
  lognonzero <- log(x.data[x.data>0])
  # Estimate mu and sigma
  theta$mu = mean(lognonzero)
  theta$sigma = var(lognonzero)
  return(theta)
}

EPSP.Binomial <- function(theta, N, n) {
  # Samples n data points from an EPSP model with parameters mu, sigma, p, and
  # N. 
  # 
  # Args:
  #   theta: Current parameter estimates. 
  #   N: The number of synaptic contacts. 
  #   n: The number of points to sample. 
  # 
  # Returns:
  #   x: The vector of points sampled from the binomial model. 
  
  SingleTrial <- function(dummy) {
    Y = rbinom(N, 1, theta$p)
    NormalIfRelease <- function(y) {
      if (y==1) {
        return(rlnorm(1, theta$mu, sqrt(theta$sigma)))
      } else {
        return(0)
      }
    }
    return(sum(apply(array(Y), 1, NormalIfRelease)))
  }
  x = rep(0, n)
  return(apply(array(x), 1, SingleTrial))
}

EPSP.SimpleBinomial <- function(theta, N, n) {
  # Samples n data points from an EPSP model with parameters mu, sigma, p, and
  # N. 
  # 
  # Args:
  #   theta: Current parameter estimates. 
  #   N: The number of synaptic contacts. 
  #   n: The number of points to sample. 
  # 
  # Returns:
  #   x: The vector of points sampled from the simple binomial model. 
  
  SingleTrial <- function(dummy) {
    Y = rbinom(N, 1, theta$p)
    MuIfRelease <- function(y) {
      if (y==1) {
        return(theta$mu)
      } else {
        return(0)
      }
    }
    return(sum(apply(array(Y), 1, MuIfRelease)))
  }
  x = rep(0, n)
  return(apply(array(x), 1, SingleTrial))
}

EPSP.BinomialUniform <- function(theta, N, n) {
  # Samples n data points from an EPSP model with parameters mu, width, p, and
  # N. 
  # 
  # Args:
  #   theta: Current parameter estimates. 
  #   N: The number of synaptic contacts. 
  #   n: The number of points to sample. 
  # 
  # Returns:
  #   x: The vector of points sampled from the simple binomial model. 
  
  SingleTrial <- function(dummy) {
    Y = rbinom(N, 1, theta$p)
    UnifIfRelease <- function(y) {
      if (y==1) {
        return(runif(1, theta$mu-theta$width, theta$mu+theta$width))
      } else {
        return(0)
      }
    }
    return(sum(apply(array(Y), 1, UnifIfRelease)))
  }
  x = rep(0, n)
  return(apply(array(x), 1, SingleTrial))
}

BernoulliAssignments <- function(N) {
  # Returns {0,1}^N as a list of lists.
  #
  # Args:
  #   N: The number of assumed synaptic contact points. 
  #
  # Returns:
  #   z.N: {0,1}^N as a list of lists. 
  if (N <= 1) {
    return(c(0,1))
  }
  z.N <- NULL
  z.N.prev <- BernoulliAssignments(N-1)
  i <- 1
  for (z in z.N.prev) {
    z.N[i] <- list(c(z, 0))
    i <- i + 1
    z.N[i] <- list(c(z, 1))
    i <- i + 1
  }
  return(z.N)
}

N.z <- function(z) {
  # The number of z.j such that z.j = 1. 
  #
  # Args: 
  #   z: Assignments of the Bernoulli variables as a {0,1} list of length N.
  # 
  # Returns: 
  #   N.z: The number of z.j such that z.j = 1.
  
  sum(z)
}

JointPDF <- function(x.i, z, theta, N) {
  # Returns the joint distribution of the data and assignments of the Bernoulli
  # variables, given the parameter estimates theta, evaluated at the point 
  # (x.i, z). 
  # 
  # Args:
  #   x.i: The ith data point.
  #   z: Assignments of the Bernoulli variables as a {0,1} list of length N.
  #   theta: Current parameter estimates.  
  #   N: The number of assumed synaptic contact points. 
  # 
  # Returns:
  #   d: The joint distribution evaluated at (x.i, z).
  
  p.x.z = dlnorm(x.i, N.z(z)*theta$mu, sqrt(N.z(z)*theta$sigma))
  p.z = theta$p^(N.z(z)) - theta$p^N
  return(p.x.z * p.z)
}

EPSP.QQ.Binomial <- function(x, filename, mu, sigma, p, N, reps=500) {
  # Plots the QQ plot for the binomial at the given filename. 
  # 
  # Args:
  #   x: The n data points. 
  #   filename: The filename for the QQ plot. 
  #   mu: The mean amplitude for each of the synaptic contacts.
  #   sigma: The variance for each of the synaptic contacts. 
  #   p: The release probability for each of the synaptic contacts.
  #   N: The assumed number of synaptic contacts. 
  #
  # Returns:
  #   data: The sampled data along with the quantiles and the original data.  
  
  n <- length(x)
  data = list()
  for (k in 1:reps) {
    data[[k]] = sort(EPSP.Binomial(mu, sigma, p, N, n))
  }
  data = data.frame(data)
  SampleName <- Curry(IndexedName, name="sample")
  names(data) = apply(array(c(1:reps)), 1, SampleName)
  
  # Compute quantiles of the aggregate data
  q = quantile(unlist(data), probs = c(1:n) / (n + 1))
  x = sort(x)
  data[["quantiles"]] = q
  data[["data"]] = x
  
  # Create QQ plot
  plot <- ggplot(data, aes(x=quantiles))
  for (k in 1:reps) {
    plot <- plot + geom_point(aes_string(y=SampleName(k)), color="steelblue")
  }
  plot <- plot + geom_line(aes(y=quantiles))
  plot <- plot + geom_point(aes(y=data), color="red")
  cat("\tsaving to", filename, "\n")
  ggsave(filename)
  return(data)
}

Bootstrap.CI.Binomial <- function(theta, N, n, B=1000) {
  # Bootstraps from a model generated by parameters theta and N. From this,
  # we determine estimates for the confidence intervals via pivot confidence
  # intervals. 
  #
  # Args: 
  #   theta: Parameter estimates.
  #   N: The assumed number of synaptic contacts. 
  #   n: Sample size per parameter estimate.
  #   B: Sample size for sampling from the sampling distribution. 
  # 
  # Returns:
  #   ci: The confidence interval estimates. 
  
  SampleEstimator <- function(dummy) {
    # Estimate the parameters from n simulated data points. 
    x <- EPSP.Binomial(theta, N, n)
    return(data.frame(MOME.Binomial(x, N)))
  }
  PivotCI <- function(quantiles, theta.hat) {
    # Compute pivot confidence intervals given the sample quantiles. 
    c(2*theta.hat - quantiles[[2]], 2*theta.hat - quantiles[[1]])
  }
  # Repeat B times
  sample <- apply(array(rep(0,B)), 1, SampleEstimator)
  sample <- do.call("rbind", sample)
  ci <- list()
  ci$mu <- PivotCI(quantile(sample$mu, probs=c(0.025, 0.975), na.rm = TRUE), 
                   theta$mu)
  ci$sigma <- PivotCI(quantile(sample$sigma, probs=c(0.025, 0.975), 
                               na.rm = TRUE), theta$sigma)
  ci$p <- PivotCI(quantile(sample$p, probs=c(0.025, 0.975), na.rm = TRUE), 
                  theta$p)
  return(ci)
}

Markov.Binomial <- function(t, theta, N) {
  # Bounds on large amplitudes.
  # 
  # Args:
  #   t: Amplitude to bound. 
  #   theta: Parameters of the model. 
  #   N: Assuemd number of synaptic contacts. 
  #
  # Returns:
  #   p: Upper bound on the probability of events larger than t using Markov.
  
  (N * exp(theta$mu + theta$sigma^2/2) * theta$p) / t
}

Chebyshev.Binomial <- function(t, theta, N) {
  # Bounds on large amplitudes.
  # 
  # Args:
  #   t: Amplitude to bound. 
  #   theta: Parameters of the model. 
  #   N: Assuemd number of synaptic contacts. 
  #
  # Returns:
  #   p: Upper bound on the probability of events larger than t using 
  #     Chebyshev.
  
  (N * (exp(2*(theta$mu+theta$sigma^2)) - exp(2*theta$mu+theta$sigma^2)) 
   * theta$p) / t^2
}
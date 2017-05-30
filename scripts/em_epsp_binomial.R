# Taisuke Yasuda
#
# This file implements the EM algorithm for inferring the mu, sigma, and p for
# the "binomial distibution" model of the postsynaptic potential distribution. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
source("./scripts/em_epsp_help.R")

# Helper functions for the main routine, EMBinomial

InitEMBinomial <- function(x.mean, x.var, x.fail.rate, N) {
  # Provides a randomly generated initialization to the EM algorithm MLE 
  # estimates using the sample mean, sample variance, and the failure rate of 
  # the data. 
  #
  # Args:
  #   x.mean: Sample mean of the data.
  #   x.var: Sample variance of the data. 
  #   x.fail.rate: Failure rate of the data. 
  #   N: The number of assumed synaptic contact points. 
  # 
  # Returns:
  #   theta0: A randomly generated initialization to the EM algorithm as a list
  #     with the fields theta0$mu, theta$sigma, and theta$p. 
  
  theta0 <- NULL
  # Compute estimate of p
  x.estimate.p <- 1 - x.fail.rate^(1/N)
  x.estimate.mu <- x.mean / (N*x.estimate.p)
  x.estimate.sigma <- x.var / (N*x.estimate.p)
  theta0$mu <- runif(1, 0, 2*x.estimate.mu)
  theta0$sigma <- runif(1, 0, 2*x.estimate.sigma)
  theta0$p <- runif(1, 0, max(1,2*x.estimate.p))
  return(theta0)
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

p <- function(x.i, z, theta, N) {
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
  #   p: The joint distribution evaluated at (x.i, z).
  
  dnorm(x.i, N.z(z)*theta$mu, N.z(z)*theta$sigma) * (p^(N.z(z)) - p^N)
}

Q <- function(x.i, theta, N) {
  # Returns the ith Q function
  # 
  # Args: 
  #   x.i: The ith data point. 
  #   theta: Current parameter estimates.
  #   N: The number of assumed synaptic contact points. 
  # 
  # Returns: 
  #   Q.i: The ith Q function.
  
  z.N <- BernoulliAssignments(N)
  Q.i <- function(y) {
    d = 0
    for (z in z.N) {
      d = d + p(x.i, z, theta, N)
    }
    return(p(x.i, y, theta, N) / d)
  }
  return(Q.i)
}

Argmax.mu <- function(x.data, all.Q.i, N) {
  # Returns the argmax of the likelihood function for mu. 
  #
  # Args:
  #   x.data: The n data points.
  #   all.Q.i: The results from the expectation step.
  #   N: The number of assumed synaptic contact points. 
  #
  # Returns:
  #   mu: The argmax of the likelihood function for mu. 
  
  z.N = BernoulliAssignments(N)
  n = length(x.data)
  d = 0
  for (i in 1:n) {
    for (z in z.N) {
      d = d + all.Q.i[i](z) * N.z(z)
    }
  }
  return(sum(x.data) / d)
}

Argmax.sigma <- function(x.data, all.Q.i, N, mu) {
  # Returns the argmax of the likelihood function for sigma. 
  #
  # Args:
  #   x.data: The n data points.
  #   all.Q.i: The results from the expectation step.
  #   N: The number of assumed synaptic contact points. 
  #   mu: The argmax for mu.
  #
  # Returns:
  #   sigma: The argmax of the likelihood function for sigma. 

  z.N = BernoulliAssignments(N)
  n = length(x.data)
  d = 0
  for (i in 1:n) {
    for (z in z.N) {
      x.i = x.data[i]
      d = d + all.Q.i[i](z) * ((x.i - N.z(z) * mu) / N.z(z))^2
    }
  }
  return(d / n)
}

Argmax.p <- function(x.data, all.Q.i, N) {
  # Returns the argmax of the likelihood function for p. 
  #
  # Args:
  #   x.data: The n data points.
  #   all.Q.i: The results from the expectation step.
  #   N: The number of assumed synaptic contact points. 
  #
  # Returns:
  #   mu: The argmax of the likelihood function for p. 
  
  z.N = BernoulliAssignments(N)
  n = length(x.data)
  d = 0
  for (i in 1:n) {
    for (z in z.N) {
      d = d + all.Q.i[i](z) * N.z(z)
    }
  }
  return(d / (N * n))
}

LogLikelihood <- function(x.data, N, theta) {
  # The log likelihood of the parameters theta. 
  #
  # Args:
  #   x.data: The n data points.
  #   N: The number of assumed synaptic contact points. 
  #   theta: Current parameter estimates. 
  
  z.N = BernoulliAssignments(N)
  d = 0
  for (x.i in x.data) {
    m = 0
    for (z in z.N) {
      m = m + p(x.i, z, theta, N)
    }
    d = d + log(m, base = exp(1))
  }
  return(d)
}

# Main routine

EMBinomial <- function(x, N, opts) {
  # Uses the EM algorithm to estimate the MLE of the "binomial distribution"
  # model of the postsynaptic potential distribution. 
  #
  # Args:
  #   x: A dataframe containing the n data points.
  #   N: The number of assumed synaptic contact points. 
  #   opts: A list containing optional arguments for the algorithm. The fields
  #     of the list are as follows:
  #       em.precision: The threshold for determining convergence of the
  #         EM algorithm.
  #       em.iter: The maximum number of iterations allowed for the EM
  #         algorithm. 
  #       em.rep: The number of repetitions of the EM algorithm (each time
  #         starting from a random initialization). 
  #
  # Returns:
  #   theta: A list of vectors containing the MLE estimates for the mu,
  #     sigma^2, and p, respectively. The three fields can be accessed by
  #     theta$mu, theta$sigma, theta$p, respectively.
  #   likelihood: The loglikelihood of the parameters. 
  
  # Populate options
  if (missing(opts)) {
    opts <- NULL
  }
  opts <- PopulateIfNull(opts, "em.precision", 0.00001)
  opts <- PopulateIfNull(opts, "em.iter", 10000)
  opts <- PopulateIfNull(opts, "em.rep", 10)
  
  # Populate and initialize variables
  n <- length(x)
  max.likelihood <- 0
  max.theta <- NULL
  curr.likelihood <- 0
  curr.theta <- NULL
  prev.likelihood <- 0
  x.data <- unlist(x)
  x.mean <- mean(x.data)
  x.var <- var(x.data)
  x.fail.freq <- as.data.frame(table(x.data))
  x.fail.index <- which(x.fail.freq["x.data"] == 0)
  x.fail.count <- 0
  if (is.na(x.fail.index)) {
    x.fail.index <- -1
    x.fail.count <- 0
  } else {
    x.fail.count <- x.fail.freq[x.fail.index,"Freq"]
  }
  x.fail.rate <- x.fail.count / n
  z.N <- BernoulliAssignments(N)
  
  # Repeat the algorithm opts$em.rep times and choose the best MLE estimate.
  for (k in 1:opts$em.rep) {
    # Initialization
    curr.theta <- InitEMBinomial(x.mean, x.var, x.fail.rate, N)
    for (l in 1:opts$em.iter) {
      # Expectation step
      all.Q.i <- apply(array(x.data), 1, Q)
      # Maximization step
      curr.theta$mu = Argmax.mu(x.data, all.Q.i, N)
      curr.theta$sigma = Argmax.sigma(x.data, all.Q.i, N, curr.theta$mu)
      curr.theta$p = Argmax.p(x.data, all.Q.i, N)
      curr.likelihood = LogLikelihood(x.data, N, curr.theta)
      if (abs(curr.likelihood - prev.likelihood) < opts$em.precision) {
        break
      }
      prev.likelihood = curr.likelihood
    }
    if (curr.likelihood > max.likelihood) {
      max.likelihood = curr.likelihood
      max.theta = curr.theta
    }
  }
  return(c(list(max.theta), max.likelihood))
}
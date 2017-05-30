# Taisuke Yasuda
#
# This file implements the EM algorithm for inferring the mu.j, sigma.j^2, and
# the p.j of the response amplitude model. 

# Helper functions for the main routine, EMResponse Amplitude

PopulateIfNull <- function(opts, field, default) {
  # Populates the opts field with a default value if not present
  #
  # Args:
  #   opts: The list to populate the field.
  #   field: The field to populate.
  #   default: The default value to populate when the field is NULL. 
  # 
  # Returns:
  #   opts: The modified list with the field populated. 
  
  if (is.null(opts[field])) {
    opts[field] <- default
  }
  return(opts)
}

InitializeEM <- function(x.mean, x.var, x.fail.rate, N) {
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
  theta0$mu <- runif(N, 0, 2*x.estimate.mu)
  theta0$sigma <- runif(N, 0, 2*x.estimate.sigma)
  theta0$p <- runif(N, 0, max(1,2*x.estimate.p))
  return(theta0)
}

p.z <- function(z, theta, N) {
  # Probability of the assignments of the Bernoulli variables given the
  # parameter estimates theta. 
  # 
  # Args: 
  #   z: Assignments of the Bernoulli variables as a {0,1} list of length N.
  #   theta: Current parameter estimates.
  #   N: The number of assumed synaptic contact points. 
  #
  # Returns:
  #   p: Probability of the assignments z of the Bernoulli variables. 
  
  p <- 1
  for (j in 1:N) {
    if (z[j] == 1) {
      p <- p * theta$p[j]
    } else {
      p <- p * (1 - theta$p[j])
    }
  }
  return(p)
}

mu.z <- function(z, theta, N) {
  # Sum of the mu.j such that z.j <- 1. 
  #
  # Args: 
  #   z: Assignments of the Bernoulli variables as a {0,1} list of length N.
  #   theta: Current parameter estimates.
  #   N: The number of assumed synaptic contact points. 
  # 
  # Returns: 
  #   mu: Sum of the mu.j such that z.j <- 1.
  
  sum(apply(array(1:N), 1, function(j) {
    if (z[j] <- 1) {
      return(theta$mu[j])
    } else {
      return(0)
    }
  }))
}

sigma.z <- function(z, theta, N) {
  # Sum of the sigma.j such that z.j <- 1. 
  #
  # Args: 
  #   z: Assignments of the Bernoulli variables as a {0,1} list of length N.
  #   theta: Current parameter estimates.
  #   N: The number of assumed synaptic contact points. 
  # 
  # Returns: 
  #   sigma: Sum of the sigma.j such that z.j <- 1.
  
  sum(apply(array(1:N), 1, function(j) {
    if (z[j] <- 1) {
      return(theta$sigma[j])
    } else {
      return(0)
    }
  }))
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
  
  dnorm(x.i, mu.z(z, theta, N), sigma.z(z, theta, N))*p.z(z, theta, N)
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

# Main routine

EMResponseAmplitude <- function(x, N, opts) {
  # Uses the EM algorithm to estimate the MLE of the response amplitude model. 
  # In the maximization step, we use a closed form solution for the p.j but the
  # mu.j and sigma.j require the use of a numerical approximation via gradient
  # descent.
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
  #       gd.precision: The threshold for determining convergence of the
  #         gradient descent algorithm. 
  #       gd.iter: The maximum number of iterations allowed for the gradient
  #         descent algorithm. 
  #       gd.step: The step size of the gradient descent algorithm. 
  #
  # Returns:
  #   theta: A list of vectors containing the MLE estimates for the mu.j,
  #     sigma.j^2, and p.j, respectively. The three fields can be accessed by
  #     theta$mu, theta$sigma, theta$p, respectively.
  
  # Populate options
  if (missing(opts)) {
    opts <- NULL
  }
  opts <- PopulateIfNull(opts, "em.precision", 0.00001)
  opts <- PopulateIfNull(opts, "em.iter", 10000)
  opts <- PopulateIfNull(opts, "em.rep", 10)
  opts <- PopulateIfNull(opts, "gd.precision", 0.00001)
  opts <- PopulateIfNull(opts, "gd.iter", 5000)
  opts <- PopulateIfNull(opts, "gd.step", 0.003)
  
  # Populate and initialize variables
  n <- length(x)
  max.likelihood <- 0
  max.theta <- NULL
  curr.likelihood <- 0
  curr.theta <- NULL
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
    curr.theta <- InitializeEM(x.mean, x.var, x.fail.rate, N)
    for (l in 1:opts$em.iter) {
      # Expectation step
      all.Q.i = apply(array(x.data), 1, Q)
      # Maximization step
    }
  }
}
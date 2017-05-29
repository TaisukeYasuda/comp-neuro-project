# Taisuke Yasuda
#
# This file implements the EM algorithm for inferring the mu_j, sigma_j^2, and
# the p_j of the response amplitude model. 

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
    opts[field] = default
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
  #   N: the number of assumed synaptic contact points. 
  # 
  # Returns:
  #   theta0: A randomly generated initialization to the EM algorithm as a list
  #     with the fields theta0$mu, theta$sigma, and theta$p. 
  
  theta0 = NULL
  # Compute estimate of p
  x.p = 1 - x.fail.rate^(1/N)
  theta0$mu = runif(N, 0, 2*x.mean)
  theta0$sigma = runif(N, 0, 2*x.var)
  theta0$p = runif(N, 0, max(1,2*x.p))
  return theta0
}

# Main routine

EMResponseAmplitude <- function(x, N, opts) {
  # Uses the EM algorithm to estimate the MLE of the response amplitude model. 
  # In the maximization step, we use a closed form solution for the p_j but the
  # mu_j and sigma_j require the use of a numerical approximation via gradient
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
  #   theta: A list of vectors containing the MLE estimates for the mu_j,
  #     sigma_j^2, and p_j, respectively. The three fields can be accessed by
  #     theta$mu, theta$sigma, theta$p, respectively.
  
  # Populate options
  if (missing(opts)) {
    opts = NULL
  }
  opts = PopulateIfNull(opts, "em.precision", 0.00001)
  opts = PopulateIfNull(opts, "em.iter", 10000)
  opts = PopulateIfNull(opts, "em.rep", 10)
  opts = PopulateIfNull(opts, "gd.precision", 0.00001)
  opts = PopulateIfNull(opts, "gd.iter", 5000)
  opts = PopulateIfNull(opts, "gd.step", 0.003)
  
  # Populate and initialize variables
  n = length(x)
  max.likelihood = 0
  max.theta = NULL
  curr.likelihood = 0
  curr.theta = NULL
  x.data = unlist(x)
  x.mean = mean(x.data)
  x.var = var(x.data)
  x.fail.freq = as.data.frame(table(x.data))
  x.fail.index = which(x.fail.freq["x.data"] == 0)
  x.fail.count = 0
  if (is.na(x.fail.index)) {
    x.fail.index = -1
    x.fail.count = 0
  } else {
    x.fail.count = x.fail.freq[x.fail.index,"Freq"]
  }
  x.fail.rate = x.fail.count / n
  
  # Repeat the algorithm opts$em.rep times and choose the best MLE estimate.
  for (k in 1:opts$em.rep) {
    # Initialization
    curr.theta = InitializeEM(x.mean, x.var, x.fail.rate, N)
  }
}
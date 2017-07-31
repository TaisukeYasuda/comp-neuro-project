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

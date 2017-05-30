# Taisuke Yasuda
#
# This file implements the useful functions implementing EM algorithms for
# various models for the postsynaptic potential distribution. 

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
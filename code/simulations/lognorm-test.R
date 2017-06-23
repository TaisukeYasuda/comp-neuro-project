# Testing the speed of the lognormal random number generator in R.

nrolls <- 1000000
p <- rlnorm(nrolls, 0, 1)

print("done")

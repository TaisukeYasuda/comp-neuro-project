# Testing the speed of the lognormal random number generator in R.

nrolls <- 10^6;
for (i in 1:100) {
    p <- rlnorm(nrolls, 0, 1)
}

print("done")

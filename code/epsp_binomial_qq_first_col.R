# Taisuke Yasuda
#
# Plots and saves the qq plots of the first trials of the epsp data. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
source("./scripts/filenames.R")
source("./scripts/epsp_binomial_lib.R")

file.names <- dir("data/epsp-data-first-col/", pattern="*.csv")
maxN = 10

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("data/epsp-data-first-col/", filename, sep=""),
                   header=TRUE, sep=",")
  x = unlist(file[2])
  p.f = SampleFailureRate(x)
  for (N in 1:maxN) {
    cat("\tN =", N, "\n")
    # For each N, compute the MOME parameter estimates
    p = MOME.p(p.f, N)
    mu = MOME.mu(x, N, p)
    sigma = MOME.sigma(x, N, p)
    # Save the QQ plots
    EPSP.QQ.Binomial(x, paste("plots/epsp-binomial/mome/qq/N=", toString(N), 
                              "/", fileroot, ".pdf", sep=""), 
                     mu, sigma, p, N)
  }
}


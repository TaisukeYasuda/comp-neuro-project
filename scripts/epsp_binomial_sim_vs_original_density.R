# Taisuke Yasuda
#
# This file plots simulations of the model with parameters as MOME estimates
# against the original data. We simulate many many points instead of the same
# number as the original data. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./scripts/epsp_binomial_lib.R")
source("./scripts/filenames.R")

# Functions for executing the task

PlotSim <- function(x, N, filename) {
  # Tests the MOME estimates by simulating from the model and then plotting the
  # histogram for the simulated data from the data. 
  # 
  # Args:
  #   x: The n data points.
  #   N: Assumed number of synaptic contacts. 
  #   filename: The filename for saving the plot. 
  
  n <- length(x)
  cat("N =", N, "\n")
  cat("Trials sampled:", n, "\n")
  theta.hat <- MOME.Binomial(x, N)
  cat("Estimated parameters: (mu, sigma, p) = (", theta.hat$mu, ", ", 
      theta.hat$sigma, ", ", theta.hat$p, ")\n", sep="")
  sim <- EPSP.Binomial(theta.hat, N, 10000)
  ci <- Bootstrap.CI.Binomial(theta.hat, N, n, B=100)
  cat("Confidence intervals: (mu, sigma, p) = ([", ci$mu[1], ", ", 
      ci$mu[2], "], [", ci$sigma[1], ", ", ci$sigma[2], "], [", 
      ci$p[1], ", ", ci$p[2], "])\n", sep="")
  
  # Plot the result if filename is specified
  if (!missing(filename)) {
    x <- data.frame(data=x, type="original")
    sim <- data.frame(data=sim, type="sim")
    plot <- ggplot(rbind(x, sim), aes(data, fill=type))
    plot <- plot + geom_histogram(alpha=0.5, aes(y=..density..), 
                                  position='identity')
    plot <- plot + labs(title=paste("Simulated Density of", fileroot), 
                        x="Amplitude (mV)", y="Density")
    ggsave(filename)
  }
}

# Script

cat("################################################################\n")
cat("Computing MOME estimates and simulating from them.\n")
cat("################################################################\n\n")

file.names <- dir("data/epsp-data/", pattern="*.csv")
maxN = 12
folder.plots <- "./plots/epsp-binomial/mome/sim-vs-original-density/"

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  # Extract the data columns
  file <- file[2:11]
  
  folder.cell <- paste(folder.plots, fileroot, "/", sep="")
  # Check existence of folder for saving results
  if (file.exists(folder.cell)) {
    cat("Error: directory '", folder.cell, "'exists already.", sep="")
  } else {
    dir.create(folder.cell)
    
    for (N in 1:maxN) {
      cat("\tN = ", N, "\n", sep="")
      folder.N <- paste(folder.cell, "N=", N, "/", sep="")
      # Check existence of folder for saving results
      if (file.exists(folder.N)) {
        cat("Error: directory '", folder.N, "'exists already.", sep="")
      } else {
        dir.create(folder.N)
        for (l in 1:length(file)) {
          cat("\t\tspike ", l, "\n", sep="")
          x <- unlist(file[l])
          PlotSim(x, N, paste(folder.N, "spike", l, ".pdf", sep=""))
          cat("\n")
        }
      }
    }
  }
}

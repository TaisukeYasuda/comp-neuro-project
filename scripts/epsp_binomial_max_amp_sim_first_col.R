# Taisuke Yasuda
#
# This file plots the bounds on the max amplitude probabilities across various
# values of N for the first spike (col). 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./scripts/epsp_binomial_lib.R")
source("./scripts/filenames.R")

# Functions for executing the task

MaxAmpSim <- function(x, N, m, filename) {
  # Tests the MOME estimates by simulating from the model and then plotting 
  # the probability of large amplitudes via Monte Carlo estimation. 
  # 
  # Args:
  #   x: The n data points.
  #   N: Assumed number of synaptic contacts. 
  #   m: Number of points to sample for the Monte Carlo estimate. 
  #   filename: The filename for saving the plot. 
  #   
  #  Returns:
  #   A: Summary of results. 
  
  n <- length(x)
  cat("N =", N, "\n")
  cat("Trials sampled:", n, "\n")
  t <- max(x)
  theta.hat <- MOME.Binomial(x, N)
  cat("Estimated parameters: (mu, sigma, p) = (", theta.hat$mu, ", ", 
      theta.hat$sigma, ", ", theta.hat$p, ")\n", sep="")
  ci <- Bootstrap.CI.Binomial(theta.hat, N, n, B=100)
  upper <- list(mu=ci$mu[2], sigma=ci$sigma[2], p=ci$p[2])
  cat("Confidence intervals: (mu, sigma, p) = ([", ci$mu[1], ", ", 
      ci$mu[2], "], [", ci$sigma[1], ", ", ci$sigma[2], "], [", 
      ci$p[1], ", ", ci$p[2], "])\n", sep="")
  cat("Maximum amplitude: ", t, "\n", sep="")
  
  # Monte Carlo estimate
  sample <- EPSP.Binomial(theta.hat, N, m)
  mc.est <- length(sample[sample >= t]) / m
  
  # Summary of results
  A = list(prob=1/n, mc.est=mc.est)
  cat("Observed Probability: ", A$prob, "\n", sep="")
  cat("Monte Carlo Estimate: ", A$mc.est, "\n", sep="")
  
  # Plot the result if filename is specified
  if (!missing(filename)) {
    df <- data.frame(cols=c("prob", "mc.est"), 
                     val=unlist(A))
    plot <- ggplot(df) + geom_col(aes(cols, val, fill=cols))
    plot <- plot + labs(title="Bounds on Tail Probabilities", x="Tests", 
                        y="Probability")
    plot <- plot + scale_fill_discrete(name="Bounding Method", 
                                       breaks=c("prob", "mc.est"), 
                                       labels=c("Observed", 
                                                "Monte Carlo Estimate"))
    plot <- plot + theme(axis.text.x=element_text(angle=45))
    ggsave(filename)
  }
  return(A)
}

PlotResults <- function(results, filename) {
  # Plots the results. 
  # 
  # Args:
  #   results: List of lists containing the bounds.  
  #   filename: Filename to save the plot. 
  
  m <- length(results)
  # Reformat data
  results <- data.frame(do.call(rbind, lapply(results, unlist)))
  # Plot results
  df <- data.frame(trials=c(1:m), 
                   type=factor(c(rep("prob", m), rep("mc.est", m)), 
                               levels=c("prob", "mc.est")), 
                   val=c(results$prob, results$mc.est))
  plot <- ggplot(df, aes(color=type, alpha=type))
  plot <- plot + geom_line(aes(x=trials, y=val))
  plot <- plot + labs(title="Bounds on Tail Probabilities", x="N", 
                      y="Probability")
  plot <- plot + scale_color_discrete(name="Bounding Method", 
                                      breaks=c("prob", "mc.est"), 
                                      labels=c("Observed Probability", 
                                               "Monte Carlo Estimate"))
  plot <- plot + scale_alpha_manual(values=c(1, 0.2), guide=FALSE)
  ggsave(filename)
}

# Script

cat("################################################################\n")
cat("Computing and plotting max amplitude probability bounds for the\n")
cat("first spike.\n")
cat("################################################################\n\n")

file.names <- dir("data/epsp-data/", pattern="*.csv")
maxN = 12
folder.plots <- "./plots/epsp-binomial/mome/max-amp-sim-first-col/"

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  # Extract the data columns
  file <- file[2:11]
  
  results <- list()
  for (N in 1:maxN) {
    # Analysis for just the first column
    x <- unlist(file[1])
    results[[N]] <- MaxAmpSim(x, N, 100000)
  }
  PlotResults(results, paste(folder.plots, fileroot, ".pdf", sep=""))
}

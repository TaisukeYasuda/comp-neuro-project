# Taisuke Yasuda
#
# This file plots the bounds on the max amplitude probabilities across spike
# and the observed probability. 

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
    plot <- plot + labs(title="Bounds on Tail Probabilities", x="Bound", 
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
  plot <- plot + labs(title="Bounds on Tail Probabilities", x="Tests", 
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
cat("Computing and plotting max amplitude probability bounds.\n")
cat("################################################################\n\n")

file.names <- dir("data/epsp-data/", pattern="*.csv")
maxN = 12
folder.plots <- "./plots/epsp-binomial/mome/max-amp-sim/"

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
        results <- list()
        for (l in 1:length(file)) {
          cat("\t\tspike ", l, "\n", sep="")
          x <- unlist(file[l])
          results[[l]] <- MaxAmpSim(x, N, 10000, paste(folder.N, "spike", l, 
                                                        ".pdf", sep=""))
          cat("\n")
        }
        PlotResults(results, paste(folder.N, "max-amp-sim.pdf", sep=""))
      }
    }
  }
}

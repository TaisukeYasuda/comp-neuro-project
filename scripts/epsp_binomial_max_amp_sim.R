# Taisuke Yasuda
#
# This file plots the bounds on the max amplitude probabilities across spike
# and the observed probability. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./scripts/epsp_binomial_lib.R")
source("./scripts/filenames.R")

# Functions for executing the task

MaxAmpProbability <- function(x, N, filename) {
  # Tests the MOME estimates by simulating from the model and then plotting 
  # the probability of large amplitudes via the Markov and Chebyshev
  # inequalities. 
  # 
  # Args:
  #   x: The n data points.
  #   N: Assumed number of synaptic contacts. 
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
  # Sumarry of results
  A = list(prob=1/n, 
           markov.est=Markov.Binomial(t, theta.hat, N),
           markov.upb=Markov.Binomial(t, upper, N),
           chebyshev.est=Chebyshev.Binomial(t, theta.hat, N),
           chebyshev.upb=Chebyshev.Binomial(t, upper, N))
  cat("Observed Probability: ", A$prob, "\n", sep="")
  cat("Markov bound (estimator): ", A$markov.est, "\n", sep="")
  cat("Markov bound (upper bound): ", A$markov.upb, "\n", sep="")
  cat("Chebyshev bound (estimator): ", A$chebyshev.est, "\n", sep="")
  cat("Chebyshev bound (upper bound): ", A$chebyshev.upb, "\n", sep="")
  
  # Plot the result if filename is specified
  if (!missing(filename)) {
    df <- data.frame(cols=c("prob", "markov.est", "markov.upb", 
                            "chebyshev.est", "chebyshev.upb"), 
                     type=c("emp", "est", "upb", "est", "upb"), 
                     val=unlist(A))
    plot <- ggplot(df, aes(fill=type)) + geom_col(aes(cols,val))
    plot <- plot + labs(title="Bounds on Tail Probabilities", x="Bound", 
                        y="Probability")
    plot <- plot + scale_fill_discrete(name="Parameter Origin", 
                                       breaks=c("emp", "est", "upb"), 
                                       labels=c("Observed", "MOME Estimate", 
                                                "CI Upper Bound"))
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
                   type=factor(c(rep("prob", m), rep("markov.est", m), 
                                 rep("markov.upb", m), 
                                 rep("chebyshev.est", m),
                                 rep("chebyshev.upb", m)), 
                               levels=c("prob", "markov.est", "markov.upb", 
                                        "chebyshev.est", 
                                        "chebyshev.upb")), 
                   val=c(results$prob, results$markov.est, results$markov.upb, 
                         results$chebyshev.est, results$chebyshev.upb))
  plot <- ggplot(df, aes(color=type, alpha=type))
  plot <- plot + geom_line(aes(x=trials, y=val))
  plot <- plot + labs(title="Bounds on Tail Probabilities", x="Tests", 
                      y="Probability")
  plot <- plot + scale_color_discrete(name="Bounding Method", 
                                      breaks=c("prob", "markov.est", 
                                               "markov.upb",
                                               "chebyshev.est",
                                               "chebyshev.upb"), 
                                      labels=c("Observed Probability", 
                                               "Markov Estimate", 
                                               "Markov Upper Bound",
                                               "Chebyshev Estimate",
                                               "Chebyshev Upper Bound"))
  plot <- plot + scale_alpha_manual(values=c(1, rep(0.2,6)), guide=FALSE)
  ggsave(filename)
}

# Script

cat("################################################################\n")
cat("Computing and plotting max amplitude probability bounds.\n")
cat("################################################################\n\n")

file.names <- dir("data/epsp-data/", pattern="*.csv")
maxN = 12
folder.plots <- "./plots/epsp-binomial/mome/max-amp/"

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
          results[[l]] <- MaxAmpProbability(x, N, paste(folder.N, "spike", l, 
                                                        ".pdf", sep=""))
          cat("\n")
        }
        PlotResults(results, paste(folder.N, "max-amp.pdf", sep=""))
      }
    }
  }
}

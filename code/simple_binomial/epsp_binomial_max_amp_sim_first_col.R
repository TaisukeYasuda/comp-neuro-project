# Taisuke Yasuda
# 
# Simple Binomial: Max Amp Sim
#
# This file plots the bounds on the max amplitude probabilities across various
# values of N for the first spike (col), for a simple binomial model instead of
# the compound model using a lognormal distribution at each contact. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./code/epsp_binomial_lib.R")
source("./code/filenames.R")

# Functions for executing the task

MaxAmpSim <- function(x, N, filename) {
  # Tests the MOME estimates computing the probability of the largest events
  # under the model. 
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
  theta.hat <- MOME.SimpleBinomial(x, N)
  cat("Estimated parameters: (mu, sigma, p) = (", theta.hat$mu, ", ", 
      theta.hat$sigma, ", ", theta.hat$p, ")\n", sep="")
  cat("Maximum amplitude: ", t, "\n", sep="")
  
  # \Pr[X\geq t] = 1 - Pr[X<t] 
  #              = 1 - \sum_{i=0}^{\lfloor t\rfloor}\binom{n}ip^i(1-p)^{n-i}
  pred.prob <- function(t) {
    k = floor(t / theta.hat$mu) # number of amplitudes needed to reach t
    if (k > N) {
      return(0) # if we need more than N, we're out of business
    }
    p <- theta.hat$p
    prob <- 0
    for (i in 0:k) {
      prob <- prob + choose(N,i)*p^i*(1-p)^(N-i)
    }
    return(max(0,1-prob))
  }
  pred.prob <- pred.prob(t)
  
  # Summary of results
  A = list(prob=1/n, pred.prob=pred.prob)
  cat("Observed Probability: ", A$prob, "\n", sep="")
  cat("Predicted Probability: ", A$pred.prob, "\n", sep="")
  
  # Plot the result if filename is specified
  if (!missing(filename)) {
    df <- data.frame(cols=c("prob", "pred.prob"), 
                     val=unlist(A))
    plot <- ggplot(df) + geom_col(aes(cols, val, fill=cols))
    plot <- plot + expand_limits(y=0)
    plot <- plot + labs(title="Bounds on Tail Probabilities", x="Tests", 
                        y="Probability")
    plot <- plot + scale_fill_discrete(name="Bounding Method", 
                                       breaks=c("prob", "pred.prob"), 
                                       labels=c("Observed", 
                                                "Predicted"))
    plot <- plot + theme(axis.text.x=element_text(angle=45))
    plot <- plot + theme_bw()
    plot <- plot + theme(axis.text=element_text(size=20),
                         axis.title=element_text(size=20),
                         legend.text=element_text(size=13),
                         panel.border=element_blank(), 
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(), 
                         axis.line=element_line(colour = "black"))
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
                   type=factor(c(rep("prob", m), rep("pred.prob", m)), 
                               levels=c("prob", "pred.prob")), 
                   val=c(results$prob, results$pred.prob))
  plot <- ggplot(df, aes(color=type, alpha=type))
  plot <- plot + geom_line(aes(x=trials, y=val))
  plot <- plot + expand_limits(y=0)
  plot <- plot + labs(title="Bounds on Tail Probabilities", x="N", 
                      y="Probability")
  plot <- plot + scale_color_discrete(name="Bounding Method", 
                                      breaks=c("prob", "pred.prob"), 
                                      labels=c("Observed Probability", 
                                               "Predicted"))
  plot <- plot + scale_x_discrete(limits=1:m)
  plot <- plot + scale_alpha_manual(values=c(1, 0.2), guide=FALSE)
  plot <- plot + theme_bw()
  plot <- plot + theme(axis.text=element_text(size=20),
                       axis.title=element_text(size=20),
                       legend.text=element_text(size=13),
                       panel.border=element_blank(), 
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(), 
                       axis.line=element_line(colour = "black"))
  ggsave(filename)
}

# Script

cat("################################################################\n")
cat("Computing and plotting max amplitude probability bounds for the\n")
cat("first spike.\n")
cat("################################################################\n\n")

file.names <- dir("data/epsp-data/", pattern="*.csv")
maxN = 12
folder.plots <- "./plots/epsp-binomial/mome/simple_binomial/max-amp-sim-first-col/"

aggregate_data <- data.frame()
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
    results[[N]] <- MaxAmpSim(x, N)
    ratio <- data.frame(ratio=results[[N]]$pred.prob / results[[N]]$prob,
                        N=N, cell=fileroot)
    aggregate_data <- rbind(aggregate_data, ratio)
  }
  PlotResults(results, paste(folder.plots, fileroot, ".pdf", sep=""))
}
plot <- ggplot(aggregate_data, aes(color=cell, x=N, y=ratio))
plot <- plot + geom_line()
plot <- plot + labs(title="Ratio of Predicted / Observed Probabilities",
                    x="N", y="Ratio")
plot <- plot + scale_x_discrete(limits=1:maxN)
plot <- plot + theme_bw()
plot <- plot + theme(axis.text=element_text(size=20),
                     axis.title=element_text(size=20),
                     panel.border=element_blank(), 
                     panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(), 
                     axis.line=element_line(colour = "black"))
plot <- plot + coord_cartesian(ylim=c(0,1))
ggsave("./plots/epsp-binomial/mome/simple_binomial/max-amp-sim-first-col.pdf")
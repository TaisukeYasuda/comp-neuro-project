# Taisuke Yasuda
# 
# Binomial Uniform: Bootstrap Max Amp
#
# This file creates a bootstrap distribution of the max amplitude. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./code/epsp_binomial_lib.R")
source("./code/filenames.R")

NOISE <- 0.2 # half-width of uniform distribution

# Functions for executing the task

MaxAmpSim <- function(x, N, m, filename) {
  # Bootstraps the max amp distribution. 
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
  
  theta.hat$width = NOISE
  
  # Monte Carlo sample of length n, m times
  results <- c()
  for (i in 1:m) {
    sample <- EPSP.BinomialUniform(theta.hat, N, n)
    results <- c(results, max(sample))
  }
  ci <- quantile(results, probs=c(0.025, 0.975))
  
  # Summary of results
  A = list(max=t, ci=ci, sample=results)
  return(A)
}

PlotResults <- function(maximum.sim, maximum.obs, ci.sample, filename) {
  # Plots the results. 
  # 
  # Args:
  #   results: List of lists containing the bounds.  
  #   filename: Filename to save the plot. 
  
  # Plot results
  plot <- ggplot(maximum.sim, aes(x=N, y=sample)) 
  plot <- plot + geom_point(aes(color="blue", alpha=0.3))
  plot <- plot + geom_line(data=maximum.obs, aes(x=N, y=obs, color="black"))
  plot <- plot + geom_line(data=ci.sample, aes(x=N, y=lo, color="yellow"))
  plot <- plot + geom_line(data=ci.sample, aes(x=N, y=hi, color="yellow"))
  plot <- plot + labs(x="Number of Assumed Contacts", 
                      y="Maximum Observed Amplitude (mV)")
  plot <- plot + scale_x_discrete(limits=1:maxN)
  plot <- plot + scale_alpha_continuous(guide=FALSE)
  plot <- plot + scale_color_manual(values=c("black", "blue", "yellow"),
                                    guide=FALSE)
  plot <- plot + expand_limits(y=0)
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
num.trials = 1000
folder.plots <- "./plots/epsp-binomial/mome/binomial-uniform/bootstrap-max-amp-sim/"

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  # Extract the data columns
  file <- file[2:11]
  
  maximum.sim <- data.frame()
  maximum.obs <- data.frame()
  ci.sample <- data.frame()
  for (N in 1:maxN) {
    # Analysis for just the first column
    x <- unlist(file[1])
    result <- MaxAmpSim(x, N, num.trials)
    ci <- result$ci
    maximum.sim <- rbind(maximum.sim, data.frame(N=N, sample=result$sample))
    maximum.obs <- rbind(maximum.obs, data.frame(N=N, obs=result$max))
    ci.sample <- rbind(ci.sample, data.frame(N=N, lo=ci[1], hi=ci[2]))
  }
  PlotResults(maximum.sim, maximum.obs, ci.sample, 
              paste(folder.plots, fileroot, ".pdf", sep=""))
}
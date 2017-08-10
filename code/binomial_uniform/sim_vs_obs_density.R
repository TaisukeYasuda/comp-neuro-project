# Taisuke Yasuda
#
# This file plots simulations of the model with parameters as MOME estimates
# against the original data. We simulate many many points instead of the same
# number as the original data. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./code/epsp_binomial_lib.R")
source("./code/filenames.R")

NOISE <- 0.2

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
  theta.hat <- MOME.SimpleBinomial(x, N)
  theta.hat$width <- NOISE
  cat("Estimated parameters: (mu, sigma, p) = (", theta.hat$mu, ", ", 
      theta.hat$sigma, ", ", theta.hat$p, ")\n", sep="")
  sim <- EPSP.BinomialUniform(theta.hat, N, 10000)
  
  # Plot the result
  x <- data.frame(data=x, type="original")
  sim <- data.frame(data=sim, type="sim")
  plot <- ggplot(rbind(x, sim), aes(data, fill=type))
  plot <- plot + geom_histogram(alpha=0.5, aes(y=..density..), 
                                position='identity', binwidth=0.05)
  plot <- plot + labs(x="Amplitude (mV)", y="Density")
  plot <- plot + theme_bw()
  plot <- plot + theme(axis.text=element_text(size=20),
                       axis.title=element_text(size=20),
                       legend.text=element_text(size=13),
                       panel.border=element_blank(), 
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(), 
                       axis.line=element_line(colour = "black"))
  filename.N <- paste(folder.cell, "N=", N, ".pdf", sep="")
  ggsave(filename.N, units="in", width=7, height=5)
  plot <- plot + coord_cartesian(ylim=c(0,5))
  filename.N <- paste(folder.cell, "N=", N, "-zoom.pdf", sep="")
  ggsave(filename.N, units="in", width=7, height=5)
}

# Script

cat("################################################################\n")
cat("Computing MOME estimates and simulating from them.\n")
cat("################################################################\n\n")

file.names <- dir("data/epsp-data/", pattern="*.csv")
maxN = 12
folder.plots <- paste("./plots/epsp-binomial/mome/binomial-uniform/",
                      "sim-vs-obs-density/", sep="")

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  # Extract the data columns
  file <- file[2:11]
  file <- file[1]
  
  folder.cell <- paste(folder.plots, fileroot, "/", sep="")
  # Check existence of folder for saving results
  if (file.exists(folder.cell)) {
    cat("Error: directory '", folder.cell, "'exists already.", sep="")
  } else {
    dir.create(folder.cell)
    
    for (N in 1:maxN) {
      cat("\tN = ", N, "\n", sep="")
      # Check existence of folder for saving results
      cat("\t\tspike ", 1, "\n", sep="")
      x <- unlist(file)
      PlotSim(x, N)
      cat("\n")
    }
  }
}

# Taisuke Yasuda
#
# This file infers and plots the binomial model for N=1 specifically. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./code/epsp_binomial_lib.R")
source("./code/filenames.R")

# Functions for executing the task

PlotMLE <- function(x, filename) {
  # Plots the distribution at the single assumed contact point using MLE.
  # 
  # Args:
  #   x: The n data points.
  #   filename: The filename for saving the plot. 
  
  n <- length(x)
  cat("Trials sampled:", n, "\n")
  theta.hat <- MLE.SingleContact(x)
  cat("Estimated parameters: (mu, sigma, p) = (", theta.hat$mu, ", ", 
      theta.hat$sigma, ", ", theta.hat$p, ")\n", sep="")
  
  # Plot the result if filename is specified
  if (!missing(filename)) {
    df <- data.frame(data=x)
    plot <- ggplot(df, aes(x=data)) + geom_histogram(alpha=0.5, aes(y=..density..), 
                                                     position='identity')
    plot <- plot + labs(title=paste("Inference with N = 1 for", fileroot))
    x <- seq(0, 5, 0.01)
    y <- dlnorm(x, theta.hat$mu, sqrt(theta.hat$sigma)) * theta.hat$p
    plot <- plot + geom_line(data=data.frame(x=x, y=y), aes(x=x, y=y))
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
}

PlotMOME <- function(x, filename) {
  # Plots the distribution at the single assumed contact point using MOME.
  # 
  # Args:
  #   x: The n data points.
  #   filename: The filename for saving the plot. 
  
  n <- length(x)
  cat("Trials sampled:", n, "\n")
  theta.hat <- MOME.Binomial(x, 1)
  cat("Estimated parameters: (mu, sigma, p) = (", theta.hat$mu, ", ", 
      theta.hat$sigma, ", ", theta.hat$p, ")\n", sep="")
  
  # Plot the result if filename is specified
  if (!missing(filename)) {
    df <- data.frame(data=x)
    plot <- ggplot(df, aes(x=data)) + geom_histogram(alpha=0.5, aes(y=..density..), 
                                                     position='identity')
    plot <- plot + labs(title=paste("Inference with N = 1 for", fileroot))
    x <- seq(0, 5, 0.01)
    y <- dlnorm(x, theta.hat$mu, sqrt(theta.hat$sigma)) * theta.hat$p
    plot <- plot + geom_line(data=data.frame(x=x, y=y), aes(x=x, y=y))
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
}

# Script

cat("################################################################\n")
cat("Computing MOME estimates and simulating from them.\n")
cat("################################################################\n\n")

file.names <- dir("data/epsp-data/", pattern="*.csv")
folder.plots <- "./plots/epsp-binomial/mome/N=1/"

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  # Extract the data columns
  file <- file[2:11]
  file <- file[1]
  
  x <- unlist(file)
  PlotMLE(x, paste(folder.plots, fileroot, "-mle.pdf", sep=""))
  PlotMOME(x, paste(folder.plots, fileroot, "-mome.pdf", sep=""))
  cat("\n")
}

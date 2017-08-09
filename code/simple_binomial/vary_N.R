# Taisuke Yasuda
#
# Simple Binomial: ECDF Vary N
# 
# This file plots empirical cdfs of simulations of the uniform parameter model
# with varying N, for the simple binomial model. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./code/epsp_binomial_lib.R")
source("./code/filenames.R")

# Script

cat("################################################################\n")
cat("Plotting ecdfs of simulations varying N from 1 to 12.\n")
cat("################################################################\n\n")

file.names <- dir("data/epsp-data/", pattern="*.csv")
maxN = 12
folder.plots <- "./plots/epsp-binomial/mome/simple_binomial/ecdf/vary-N/"

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  # Extract the data columns
  file <- file[2:11]
  
  numSim <- 1000
  aggregate.data <- c()
  x <- unlist(file[1])
  failure.rate <- SampleFailureRate(x)
  for (N in 1:maxN) {
    # Infer parameters, simulate, and add to aggregate data
    theta.hat <- MOME.SimpleBinomial(x, N)
    x <- EPSP.SimpleBinomial(theta.hat, N, numSim)
    aggregate.data <- c(aggregate.data, x)
  }
  aggregate.data <- data.frame(x=aggregate.data, N=gl(maxN, numSim))
  plot <- ggplot(aggregate.data, aes(x=x, color=N, alpha=1)) + stat_ecdf()
  print("simulated")
  obs.data <- file[1]
  names(obs.data) <- "x"
  plot <- plot + stat_ecdf(data=obs.data, aes(x=x, color="observed", alpha=1))
  print("observed")
  plot <- plot + labs(title="Empircal CDFs Varying Across N",
                      x="Amplitude (mV)", y="Cumulative Probability")
  ecdf.colors <- heat.colors(12)
  ecdf.colors[maxN+1] = "#000000"
  plot <- plot + scale_color_manual(labels=c(1:maxN, "observed"),
                                    values=ecdf.colors)
  plot <- plot + scale_alpha_continuous(guide=FALSE)
  plot <- plot + theme_bw()
  plot <- plot + theme(axis.text=element_text(size=20),
                       axis.title=element_text(size=20),
                       panel.border=element_blank(), 
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(), 
                       axis.line=element_line(colour = "black"))
  ggsave(paste(folder.plots, fileroot, ".pdf", sep=""))
  plot <- plot + coord_cartesian(ylim=c(failure.rate, 1))
  ggsave(paste(folder.plots, fileroot, "-zoom.pdf", sep=""))
}

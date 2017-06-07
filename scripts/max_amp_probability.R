# Taisuke Yasuda
#
# Plots and saves estimates of the probabilities of the maximum amplitude of
# each cell using binomial model estimates of parameters. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
library(functional)
source("./scripts/filenames.R")
source("./scripts/epsp_binomial.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

# Specify subset of the data, title, and folder to save the plot
Title <- function(fileroot) {
  paste("Probabilities of Maximum Amplitude ", fileroot, sep="")
}
FileName <- function(fileroot) {
  paste("plots/max-amp-prob/", fileroot, ".pdf", sep="")
}

aggregate_data <- data.frame()
for (i in 1:length(file.names)) {
  # Read the data
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  file = file[2:11]
  names(file) = c(1:10)
  
  # Compute sample estimates of parameters
  fail <- apply(array(file), 2, SampleFailureRate)
  mean <- apply(array(file), 2, mean)
  var <- apply(array(file), 2, var)
  
  # Compute theoretical estimates of the probability of observing the maximum
  # amplitude of each trial
  
  # Plot the failure rates
  plot <- ggplot(data.frame("fail"=fail, "trials"=1:10), aes(x=trials, y=fail))
  plot <- plot + geom_line() + labs(title=Title(fileroot), x="spike", y="failure rate")
  plot <- plot + scale_y_continuous(limits=c(0, 1))
  ggsave(FileName(fileroot))
}

# Plot all the failure rates in one plot
plot <- ggplot(aggregate_data, aes(x=trials, y=fail, color=cell)) + geom_line()
plot <- plot + labs(title="Failure Rates over Successive Spikes", x="spike", y="failure rate")
plot <- plot + scale_y_continuous(limits=c(0,1))
ggsave("./plots/failure-rates/all.pdf")


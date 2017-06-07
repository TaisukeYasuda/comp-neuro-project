# Taisuke Yasuda
#
# Plots and saves the failure rates of each cell. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
library(functional)
source("./scripts/filenames.R")
source("./scripts/epsp_binomial.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

# Specify subset of the data, title, and folder to save the plot
Title <- function(fileroot) {
  paste("Failure Rates of ", fileroot, sep="")
}
FileName <- function(fileroot) {
  paste("plots/failure-rates/", fileroot, ".pdf", sep="")
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
  
  # Compute failure rates
  fail <- apply(array(file), 2, SampleFailureRate)
  aggregate_data = rbind(aggregate_data, data.frame(fail=fail, trials=1:10,
                                                    cell=fileroot))
  
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
ggsave("./plots/failure-rates.pdf")


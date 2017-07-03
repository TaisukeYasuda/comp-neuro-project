# Taisuke Yasuda
#
# Plots and saves the mean amplitudes vs failure rates for each trial and each
# cell. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
library(functional)
source("./code/filenames.R")
source("./code/epsp_binomial_lib.R")

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
  
  # Compute failure rates and mean amplitudes
  fail <- apply(array(file), 2, SampleFailureRate)
  means <- apply(array(file), 2, mean)
  aggregate_data = rbind(aggregate_data, data.frame(fail=fail, means=means, 
                                                    spikes=1:10, 
                                                    cell=fileroot))
}

plot <- ggplot(aggregate_data, aes(x=means, y=fail, color=cell)) + geom_point()
plot <- plot + labs(title="Mean Amplitude vs Failure Rate", 
                    x="Mean Amplitude (mV)", y="Failure Rate")
plot <- plot + scale_color_discrete(name="Cell")
plot <- plot + theme_bw()
plot <- plot + theme(panel.border=element_blank(), 
                     panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(), 
                     axis.line=element_line(colour = "black"))
ggsave("./plots/mean-amp-vs-failure-rate.pdf")
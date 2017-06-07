# Taisuke Yasuda
#
# Plots and saves the histogram of the first trials of the epsp data.

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./scripts/filenames.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

# Specify subset of the data, title, and folder to save the plot
cols = c(1:10)
Title <- function(fileroot) {
  paste("Histogram of ", fileroot, sep="")
}
FileName <- function(fileroot) {
  paste("plots/histogram-all/", fileroot, ".pdf", sep="")
}

for (i in 1:length(file.names)) {
  # Read the data
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  file = file[2:11]
  names(file) = c(1:10)
  
  # Plot the histogram
  plot <- ggplot(data.frame("data"=unlist(file[cols])), aes(x=data))
  plot <- plot + geom_histogram() + ggtitle(Title(fileroot))
  ggsave(FileName(fileroot))
}
print(plot)


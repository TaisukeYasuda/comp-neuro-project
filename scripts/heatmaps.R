# Taisuke Yasuda
#
# Plots and saves the heat map of the data. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
library(reshape2)
source("./scripts/filenames.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

# Specify subset of the data, title, and folder to save the plot
cols = c(1:10)
Title <- function(fileroot) {
  paste("Heatmap of ", fileroot, sep="")
}
FileName <- function(fileroot) {
  paste("plots/heatmaps/", fileroot, ".pdf", sep="")
}

for (i in 1:length(file.names)) {
  # Read the data
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("./data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  file <- file[2:11]
  names(file) <- c(1:10)
  file <- melt(as.matrix(file))
  names(file) <- c("Sweep", "Spike", "Amplitude")
  
  # Plot the heatmap
  plot <- ggplot(file, aes(x=Spike, y=Sweep, fill=Amplitude))
  plot <- plot + geom_tile() + ggtitle(Title(fileroot))
  ggsave(FileName(fileroot))
}


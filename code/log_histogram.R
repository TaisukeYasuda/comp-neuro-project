# Taisuke Yasuda
#
# Plots and saves the histogram of the log of the first trials of the data.

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./code/filenames.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

# Specify subset of the data, title, and folder to save the plot
cols = 1
Title <- function(fileroot) {
  paste("Histogram of Log of ", fileroot, sep="")
}
FileName <- function(fileroot) {
  paste("plots/log-histogram-first-col/", fileroot, ".pdf", sep="")
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
  file = data.frame(data=unlist(file[cols]))
  # Extract nonzero values to take log
  file = file[file$data > 0,]
  # Take log
  file = apply(array(file), 1, log)
  
  # Plot the histogram
  plot <- ggplot(data.frame("data"=file), aes(x=data))
  plot <- plot + geom_histogram() + ggtitle(Title(fileroot))
  plot <- plot + theme_bw()
  plot <- plot + theme(panel.border=element_blank(), 
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(), 
                       axis.line=element_line(colour = "black"))
  ggsave(FileName(fileroot))
}
print(plot)


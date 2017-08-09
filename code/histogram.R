# Taisuke Yasuda
#
# Plots and saves the histogram of the first trials of the epsp data.

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./code/filenames.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

# Specify subset of the data, title, and folder to save the plot
cols = c(1)
Title <- function(fileroot) {
  paste("Histogram of ", fileroot, sep="")
}
FileName <- function(fileroot) {
  paste("plots/histogram-first-col/", fileroot, ".pdf", sep="")
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
  plot <- plot + geom_histogram(binwidth=0.05)
  # plot <- plot + ggtitle(Title(fileroot))
  plot <- plot + labs(x="Amplitude (mV)", y="Number of \n Observed Amplitudes")
  plot <- plot + theme_bw()
  plot <- plot + theme(axis.text=element_text(size=30),
                       axis.title=element_text(size=30),
                       panel.border=element_blank(), 
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(), 
                       axis.line=element_line(colour = "black"))
  ggsave(FileName(fileroot), units="in", width=7, height=5)
  plot <- plot + coord_cartesian(xlim=c(0,2.5))
  ggsave(paste("plots/histogram-first-col/", fileroot, "-scaled.pdf", sep=""),
         units="in", width=7, height=5)
}
print(plot)


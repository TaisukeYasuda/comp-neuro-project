# Taisuke Yasuda
#
# Plots and saves the heat map of the data. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
library(reshape2)
library(colorRamps)
source("./code/filenames.R")

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
  names(file) <- c("sweep", "spike", "amplitude")
  file$sweep = rev(file$sweep) # reverse order for plotting
  
  # Plot the heatmap
  plot <- ggplot(file, aes(x=spike, y=sweep, fill=amplitude))
  plot <- plot + geom_tile() + ggtitle(Title(fileroot))
  plot <- plot + labs(x="Stimulus Spike Number", y="Sweep")
  plot <- plot + scale_x_discrete(limits=1:10)
  plot <- plot + scale_fill_gradientn(name="Amplitude (mV)", 
                                      colors=matlab.like(1000))
                                      #limits=c(0,7))
  plot <- plot + theme_bw()
  plot <- plot + theme(axis.text=element_text(size=20),
                       axis.title=element_text(size=20),
                       legend.title=element_text(size=20),
                       legend.text=element_text(size=20),
                       panel.border=element_blank(), 
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(), 
                       axis.line=element_line(colour = "black"))
  ggsave(FileName(fileroot))
}


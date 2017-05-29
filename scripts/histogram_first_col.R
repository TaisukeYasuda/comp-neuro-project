# Taisuke Yasuda
#
# Plots and saves the histogram of the first trials of the epsp data.

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
source("./scripts/filenames.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  pdf(paste("plots/histogram-first-col/", fileroot, ".pdf", sep=""))
  hist(unlist(file[2]),
       main=paste("Histogram of First Spike of", fileroot),
       xlab="Amplitude (mV)",
       ylab="Frequency")
  dev.off()
}


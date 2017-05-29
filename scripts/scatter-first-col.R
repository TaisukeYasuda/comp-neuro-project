# Taisuke Yasuda
#
# Plots and saves the scatter plot of the first trials of the epsp data. This is
# used to check the stationarity of our experiment across trials.

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
source("./scripts/filenames.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  pdf(paste("plots/scatter-first-col/", fileroot, ".pdf", sep=""))
  plot(X2 ~ X,
       data=file,
       main=paste("Scatter Plot of First Spike of", fileroot),
       xlab="Trial",
       ylab="Amplitude")
  abline(lm(X2 ~ X, data=file), col="blue")
  dev.off()
}


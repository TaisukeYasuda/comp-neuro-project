# Taisuke Yasuda
#
# Plots and saves the histogram of the first trials of the epsp data.

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")

# FileRoot: character -> character
# Parses a filename of the form "root.ext" and returns the root.
FileRoot <- function(filename) {
  substr(filename, 1, nchar(filename) - nchar(tools::file_ext(filename)) - 1)
}

file.names <- dir("data/epsp-data/", pattern="*.csv")

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  file <- read.csv(file=paste("data/epsp-data/",filename,sep=""),
                   header=TRUE, sep=",")
  pdf(paste("plots/histogram-first-col/",fileroot,".pdf", sep=""))
  hist(unlist(file[2]),
       main=paste("Histogram of First Trial of",fileroot),
       xlab="Amplitude (mV)",
       ylab="frequency")
  dev.off()
}


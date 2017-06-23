# Taisuke Yasuda
#
# This script extracts the first column of the data. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
source("./scripts/filenames.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  write.csv(file[2], paste("data/epsp-data-first-col/", fileroot, 
                           "-first-col.csv", sep=""))
}
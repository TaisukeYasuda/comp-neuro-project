# Taisuke Yasuda
#
# This file implements the useful functions for manipulating file names. 

FileRoot <- function(filename) {
  # Parses a filename of the form "root.ext" and returns the root.
  #
  # Args:
  #   filename: The filename of the form "root.ext".
  # 
  # Returns:
  #   root: The root of the filename.
  substr(filename, 1, nchar(filename) - nchar(tools::file_ext(filename)) - 1)
}

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
source("./scripts/filenames.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

for (i in 1:length(file.names)) {
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
}
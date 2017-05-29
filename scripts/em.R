# Taisuke Yasuda
#
# This file implements the EM algorithm for inferring the mu_j, sigma_j^2, and
# the p_j of the response amplitude model. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
source("./scripts/filenames.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

for (i in 1:length(file.names)) {
  print(FileRoot(file.names[i]))
}


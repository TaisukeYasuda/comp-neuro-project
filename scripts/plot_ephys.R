# Taisuke Yasuda
#
# This file plots the bounds the raw electrophysiological data. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
library(IgorR)
source("./scripts/filenames.R")
source("./scripts/read_ephys.R")

ephys.sweep <- "sweep16"
ephys.sweeps <- c("sweep7", "sweep8", "sweep9", "sweep10", "sweep11", 
                  "sweep12", "sweep13", "sweep14", "sweep15", "sweep16")
ephys.file <- "11sept2015e.pxp"
ephys.file.root <- FileRoot(ephys.file)
ephys.folder <- "./data/electro-data/"

results.folder <- "./plots/ephys/"

# Read the pxp file
x <- read.pxp(paste(ephys.folder, ephys.file, sep=""))
stimulus <- x[["Patch 1"]]
response <- x[["Patch 2"]]

# Construct basic data frame
df.stimulus <- SweepToDataFrame(stimulus[[ephys.sweep]])
df.response <- SweepToDataFrame(response[[ephys.sweep]])
df.stimulus$type <- "stimulus"
df.response$type <- "response"

# Plot stimulus and response together
df <- rbind(df.stimulus, df.response)
plot <- ggplot(df, aes(x=t, y=y, color=type)) + geom_line()
plot <- plot + labs(x="Time", y="Response (mV)")
if (FALSE) {
  ggsave(paste(results.folder, ephys.file.root, "-", ephys.sweep, ".pdf", 
               sep=""))
}

# Move response up
df.response$y <- df.response$y + 60
df <- rbind(df.stimulus, df.response)
plot <- ggplot(df, aes(x=t, y=y, color=type)) + geom_line()
plot <- plot + labs(x="Time", y="Response (mV)")
if (FALSE) {
  ggsave(paste(results.folder, ephys.file.root, "-", ephys.sweep, 
               "-move-stimulus.pdf", sep=""))
}

# Zoom in
plot <- plot + coord_cartesian(ylim=c(-3, 4), xlim=c(2, 2.5))
if (FALSE) {
  ggsave(paste(results.folder, ephys.file.root, "-", ephys.sweep, 
               "-move-stimulus-zoom.pdf", sep=""))
}

# Just the stimulus
plot <- ggplot(df.stimulus, aes(x=t, y=y)) + geom_line()
plot <- plot + labs(x="Time", y="Response (mV)")
if (FALSE) {
  ggsave(paste(results.folder, ephys.file.root, "-", ephys.sweep, 
               "-stimulus.pdf", sep=""))
}

# Just the response
plot <- ggplot(df.response, aes(x=t, y=y)) + geom_line()
plot <- plot + labs(x="Time", y="Response (mV)")
if (FALSE) {
  ggsave(paste(results.folder, ephys.file.root, "-", ephys.sweep, 
               "-response.pdf", sep=""))
}

df <- data.frame()
for (i in 1:10) {
  ephys.sweep <- ephys.sweeps[i]
  df.response <- SweepToDataFrame(response[[ephys.sweep]])
  df.response$y <- df.response$y + 10 * i
  df.response$sweep <- ephys.sweep
  df <- rbind(df, df.response)
}
df.stimulus <- SweepToDataFrame(stimulus[[ephys.sweep]])
df.stimulus$sweep <- "stimulus"
df <- rbind(df, df.stimulus)
plot <- ggplot(df, aes(x=t, y=y)) + geom_line(aes(color=sweep))
plot <- plot + labs(title=paste("First 10 Sweeps of", ephys.file.root), 
                    x="Time (s)", y="Amplitude (mV)")
ggsave(paste("./plots/ephys/combined/", ephys.file.root, ".pdf", sep=""))
plot <- plot + coord_cartesian(xlim=c(1.9, 2.5))
ggsave(paste("./plots/ephys/combined/", ephys.file.root, "-zoom.pdf", sep=""))

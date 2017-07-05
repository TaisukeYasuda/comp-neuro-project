# Taisuke Yasuda
#
# This file plots the bounds the raw electrophysiological data. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
library(IgorR)
source("./code/filenames.R")
source("./code/read_ephys.R")

ephys.sweep <- "sweep14"
ephys.sweeps <- c("sweep5", "sweep6", "sweep7", "sweep8", "sweep9", 
                  "sweep10", "sweep11", "sweep12", "sweep13", "sweep14")
ephys.file <- "17march2016g.pxp"
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
offset <- 5
interval <- 6
average <- SweepToDataFrame(response[[ephys.sweeps[1]]])
for (i in 1:10) {
  ephys.sweep <- ephys.sweeps[i]
  df.response <- SweepToDataFrame(response[[ephys.sweep]])
  average$y <- average$y + df.response$y
  df.response$y <- df.response$y + interval * i + offset
  df.response$sweep <- ephys.sweep
  df.response$average <- FALSE
  df <- rbind(df, df.response)
}
average$y <- (average$y / 10) + 10
average$average <- TRUE
average$sweep <- "average"
df <- rbind(df, average)

plot <- ggplot(df, aes(x=t, y=y)) + geom_line(aes(group=sweep, size=average))
plot <- plot + scale_size_manual(values=c(1,2))
plot <- plot + labs(title=paste("First 10 Sweeps of", ephys.file.root), 
                    x="Time (s)", y="Amplitude (mV)")
for (i in 1:10) {
  line_df <- data.frame(x=2+(i-1)*0.05,y=c(-80,80))
  plot <- plot + geom_line(data=line_df, linetype = "dashed", aes(x=x, y=y))
}
plot <- plot + theme_bw()

ggsave(paste("./plots/ephys/combined/", ephys.file.root, "-ns-axis.pdf", sep=""))
plot <- plot + coord_cartesian(xlim=c(1.9,2.5), ylim=c(-60,9))
ggsave(paste("./plots/ephys/combined/", ephys.file.root, "-zoom-ns-axis.pdf", 
             sep=""))
plot <- plot + coord_cartesian(xlim=NULL, ylim=NULL)

plot <- plot + theme(panel.border=element_blank(), 
                     panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(), 
                     axis.line=element_blank(),
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank())

ggsave(paste("./plots/ephys/combined/", ephys.file.root, "-ns.pdf", sep=""))
plot <- plot + coord_cartesian(xlim=c(1.9,2.5), ylim=c(-60,9))
ggsave(paste("./plots/ephys/combined/", ephys.file.root, "-zoom-ns.pdf", 
             sep=""))

if (FALSE) {
  df.stimulus <- SweepToDataFrame(stimulus[[ephys.sweep]])
  df.stimulus$sweep <- "stimulus"
  df <- rbind(df, df.stimulus)
  plot <- ggplot(df, aes(x=t, y=y)) + geom_line(aes(color=sweep))
  plot <- plot + labs(title=paste("First 10 Sweeps of", ephys.file.root), 
                      x="Time (s)", y="Amplitude (mV)")
  plot <- plot + theme_bw()
  plot <- plot + theme(panel.border=element_blank(), 
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(), 
                       axis.line=element_line(colour = "black"))
  ggsave(paste("./plots/ephys/combined/", ephys.file.root, ".pdf", sep=""))
  plot <- plot + coord_cartesian(xlim=c(1.9, 2.5))
  ggsave(paste("./plots/ephys/combined/", ephys.file.root, "-zoom.pdf", sep=""))
}

# Taisuke Yasuda
#
# Plots and saves the failure rates of each cell. 

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
library(functional)
source("./code/filenames.R")
source("./code/epsp_binomial_lib.R")

file.names <- dir("data/epsp-data/", pattern="*.csv")

# Specify subset of the data, title, and folder to save the plot
Title <- function(fileroot) {
  paste("Failure Rates of ", fileroot, sep="")
}
FileName <- function(fileroot) {
  paste("plots/failure-rates/", fileroot, ".pdf", sep="")
}

aggregate_data <- data.frame()
for (i in 1:length(file.names)) {
  # Read the data
  filename <- file.names[i]
  fileroot <- FileRoot(filename)
  print(fileroot)
  file <- read.csv(file=paste("data/epsp-data/", filename, sep=""),
                   header=TRUE, sep=",")
  file = file[2:11]
  names(file) = c(1:10)
  
  # Compute failure rates
  fail <- apply(array(file), 2, SampleFailureRate)
  aggregate_data = rbind(aggregate_data, data.frame(fail=fail, spikes=1:10,
                                                    cell=fileroot,
                                                    sweeps=length(file[1])))
  
  # Plot the failure rates
  plot <- ggplot(data.frame("fail"=fail, "spikes"=1:10), aes(x=spikes, y=fail))
  plot <- plot + geom_line() + labs(title=Title(fileroot), x="Spike Number", 
                                    y="Failure Rate")
  plot <- plot + scale_y_continuous(limits=c(0, 1))
  if (FALSE) {
    ggsave(FileName(fileroot))
  }
}

# Plot all the failure rates in one plot
plot <- ggplot(aggregate_data, aes(x=spikes, y=fail, color=cell)) + geom_line()
plot <- plot + geom_point(aes(x=spikes, y=fail, color=cell, size=40))
plot <- plot + labs(title="Failure Rates over Successive Spikes", x="Spike Number", 
                    y="Failure Rate")
plot <- plot + scale_y_continuous(limits=c(0,1))
plot <- plot + scale_x_discrete(limits=1:10)
plot <- plot + theme_bw()
plot <- plot + theme(axis.text=element_text(size=30),
                     axis.title=element_text(size=30),
                     panel.border=element_blank(), 
                     panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(), 
                     axis.line=element_line(colour = "black"))

if (TRUE) {
  ggsave("./plots/failure-rates.pdf", width=11, height=8, units="in")
}

if (TRUE) {
  # Plot the average failure rate in one plot
  averages <- apply(array(1:10), 1, function(spike) {
    mean(aggregate_data[aggregate_data$spikes == spike,]$fail)
  })
  stds <- apply(array(1:10), 1, function(spike) {
    s <- sqrt(var(aggregate_data[aggregate_data$spikes == spike,]$fail))
    m <- averages[spike]
    return(data.frame(ymin.std=m-s, ymax.std=m+s))
  })
  stds <- do.call(rbind, stds)
  ci <- apply(array(1:10), 1, function(spike) {
    quantile(aggregate_data[aggregate_data$spikes == spike,]$fail, 
             probs=c(0.025, 0.975))
  })
  ci <- data.frame(t(ci))
  names(ci) <- c("ymin", "ymax")
  df <- cbind(data.frame(x=1:10, y=averages), ci)
  df <- cbind(df, stds)
  df <- cbind(df, data.frame(dummy=c("5may2016a-ctrl", "17march2016g")))
  plot <- ggplot(df) + geom_line(aes(x=x, y=y)) + coord_cartesian("ylim"=c(0,1))
  plot <- plot + geom_point(aes(x=x, y=y, size=40, color=dummy))
  plot <- plot + geom_point(aes(x=x, y=y, size=40))
  plot <- plot + scale_x_discrete(limits=1:10)
  plot <- plot + theme_bw()
  plot <- plot + theme(axis.text=element_text(size=30),
                       axis.title=element_text(size=30),
                       panel.border=element_blank(), 
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(), 
                       axis.line=element_line(colour = "black"))
  plot <- plot + labs(title="Average Failure Rate", x="Spike Number", 
                      y="Failure Rate")
  write.csv(df, "stats.csv")
  ggsave("./plots/average-failure-rate.pdf", width=11, height=8, units="in")
  if (FALSE) { 
    plot <- plot + geom_errorbar(aes(x=x, ymin=ymin, ymax=ymax))
    ggsave("./plots/average-failure-rate-ci.pdf")
  }
  if (TRUE) { 
    plot <- plot + geom_errorbar(aes(x=x, ymin=ymin.std, ymax=ymax.std, 
                                     width=0))
    ggsave("./plots/average-failure-rate-ci-std.pdf", width=11, height=8, 
           units="in")
  }
}

# Plot the average failure rate in one plot
averages <- apply(array(1:10), 1, function(spike) {
  fails <- aggregate_data[aggregate_data$spikes == spike,]$fail
  sweeps <- aggregate_data[aggregate_data$spikes == spike,]$sweeps
  total <- sum(sweeps)
  return(fails %*% sweeps / total)
})
if (FALSE) {
  df <-data.frame(x=1:10, y=averages)
  plot <- ggplot(df) + geom_line(aes(x=x, y=y)) + coord_cartesian("ylim"=c(0,1))
  plot <- plot + labs(title="Cumulative Failure Rate", x="Spike Number", 
                      y="Failure Rate")
  plot <- plot + theme_bw()
  plot <- plot + theme(panel.border=element_blank(), 
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(), 
                       axis.line=element_line(colour = "black"))
  ggsave("./plots/cumulative-failure-rate.pdf")
}

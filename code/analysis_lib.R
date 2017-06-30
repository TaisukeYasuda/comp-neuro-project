# Taisuke Yasuda
#
# This file implements the useful functions for extracting amplitudes from pxp
# files. 

library(IgorR)
library(ggplot2)
library(R6)
source("./code/read_ephys.R")

IgorAnalysis <- R6Class("IgorAnalysis",
  public = list(
    # cumulative data
    pxp = NULL,
    stimulus.patch = NULL,
    stimulus.dfs = NULL,
    response.patch = NULL,
    response.dfs = NULL,
    # working sweep
    sweep.name = "sweep0",
    sweep.df = NULL,
    sweep.stimulus.df = NULL,
    sweep.response.df = NULL,
    # plotting settings
    xlim = c(1.9, 2.5),
    ylim = c(-5, 5),
    offset = 60,
    # recording amplitudes
    empty = data.frame(
      spike1=-1, 
      spike2=-1, 
      spike3=-1, 
      spike4=-1, 
      spike5=-1, 
      spike6=-1, 
      spike7=-1, 
      spike8=-1, 
      spike9=-1, 
      spike10=-1),
    amplitudes = list(),
    initialize = function(pxp = NULL) {
      self$pxp <- pxp
      self$stimulus.patch <- pxp[["Patch 1"]]
      self$stimulus.dfs <- private$patchToSweepDataFrames(self$stimulus.patch)
      self$response.patch <- pxp[["Patch 2"]]
      self$response.dfs <- private$patchToSweepDataFrames(self$response.patch)
      self$sweep.stimulus.df <- self$stimulus.dfs[[self$sweep.name]]
      self$sweep.stimulus.df$type <- "stimulus"
      self$sweep.response.df <- self$response.dfs[[self$sweep.name]]
      self$sweep.response.df$type <- "response"
      self$sweep.df <- rbind(self$sweep.stimulus.df, self$sweep.response.df)
      # initialize amplitudes
      for (sweep in names(self$stimulus.dfs)) {
        self$amplitudes[[sweep]] = self$empty
      }
    },
    names = function() {
      print("stimulus:")
      print(names(self$stimulus.dfs))
      print("response:")
      print(names(self$response.dfs))
    },
    set.sweep = function(i) {
      self$sweep.name <- paste("sweep", i, sep="")
      self$sweep.stimulus.df <- self$stimulus.dfs[[self$sweep.name]]
      self$sweep.stimulus.df$type <- "stimulus"
      self$sweep.response.df <- self$response.dfs[[self$sweep.name]]
      self$sweep.response.df$type <- "response"
      self$sweep.df <- rbind(self$sweep.stimulus.df, self$sweep.response.df)
      print(paste("Sweep set to", self$sweep.name))
      self$add(self$offset)
      return(self$plot())
    },
    set.xlim = function(xlim=NULL) {
      self$xlim = xlim
      return(self$plot())
    },
    set.ylim = function(ylim=NULL) {
      self$ylim = ylim
      return(self$plot())
    },
    plot = function(mode="sweep") {
      if (mode == "sweep") sweep <- self$sweep.df
      else if (mode == "stimulus") sweep <- self$sweep.stimulus.df
      else if (mode == "response") sweep <- self$sweep.response.df
      else return()
      plot <- ggplot(sweep, aes(x=t, y=y, color=type)) + geom_line()
      plot <- plot + labs(x="Time (s)", y="Amplitude (mV)")
      plot <- plot + theme_bw()
      plot <- plot + theme(panel.border=element_blank(), 
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(), 
                           axis.line=element_line(colour = "black"))
      plot <- plot + coord_cartesian(xlim=self$xlim, ylim=self$ylim)
      return(plot)
    },
    plot.original = function() {
      xlim <- self$xlim
      ylim <- self$ylim
      offset <- self$offset
      self$xlim <- NULL
      self$ylim <- NULL
      self$offset <- 0
      plot <- self$plot()
      self$xlim <- xlim
      self$ylim <- ylim
      self$offset <- offset
      return(plot)
    },
    add = function(dy=0) {
      self$sweep.response.df$y <- self$sweep.response.df$y + dy
      self$sweep.df <- rbind(self$sweep.response.df, self$sweep.stimulus.df)
      return(self$plot())
    },
    record.amp = function(spike, amp=0) {
      if (missing(spike)) {
        print(self$amplitudes[[self$sweep.name]])
      } else {
        spike.name = paste("spike", spike, sep="")
        self$amplitudes[[self$sweep.name]][[spike.name]] = amp
        print(self$amplitudes[[self$sweep.name]])
      }
    },
    export = function(filename="test.csv") {
      df = do.call(rbind, self$amplitudes)
      write.csv(df, filename)
    },
    amp.interval = function(start, length=0.05, t.start=1, t.end=3.5) {
      plot <- self$plot()
      interval <- self$sweep.response.df
      # restrict to interval
      interval <- interval[interval$t >= start & interval$t <= start + length,]
      max.volt <- max(interval$y)
      base.volt <- interval$y[1] # the amplitude at the beginning
      print(paste("Max volt:", max.volt))
      print(paste("Base volt:", base.volt))
      print(paste("Amplitude:", max.volt - base.volt))
      df.max <- data.frame(x=c(t.start, t.end), y=max.volt, type="max")
      df.min <- data.frame(x=c(t.start, t.end), y=base.volt, type="base")
      df <- rbind(df.max, df.min)
      return(plot + geom_line(data=df, aes(x=x, y=y)))
    }
  ), 
  private = list(
    getSweeps = function(patch) {
      isSweep <- function(name) {
        startsWith(name, "sweep")
      }
      return(patch[apply(array(names(patch)), 1, isSweep)])
    },
    patchToSweepDataFrames = function(patch) {
      return(lapply(private$getSweeps(patch), SweepToDataFrame))
    }
  )
)

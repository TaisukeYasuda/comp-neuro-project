# Taisuke Yasuda
#
# This file implements the useful functions for extracting amplitudes from pxp
# files. 

library(IgorR)
library(ggplot2)
source("./code/read_ephys.R")

Stimulus <- function(pxp) {
  # Returns the stimulus patch.
  #
  # Args: 
  #   pxp: The pxp file. 
  #
  # Returns:
  #   stimulus: The stimulus patch.
  return(pxp[["Patch 1"]])
}

Response <- function(pxp) {
  # Returns the response patch.
  #
  # Args: 
  #   pxp: The pxp file. 
  #
  # Returns:
  #   response: The response patch.
  return(pxp[["Patch 2"]])
}

GetSweeps <- function(patch) {
  # Returns just the sweeps of the patch. 
  #
  # Args:
  #   patch: The pxp file patch.
  # 
  # Returns:
  #   patch: A subset containing just the sweeps.
  isSweep <- function(name) {
    startsWith(name, "sweep")
  }
  return(patch[apply(array(names(patch)), 1, isSweep)])
}

PatchToSweepDataFrames <- function(patch) {
  # Returns just the sweeps, and in dataframe form.
  # 
  # Args:
  #   patch: The pxp file patch.
  #
  # Returns:
  #   patch: The sweeps in dataframe form. 
  return(lapply(GetSweeps(patch), SweepToDataFrame))
}

PlotSweep <- function(sweep) {
  # Plots the sweep using ggplot. 
  # 
  # Args:
  #   sweep: A sweep dataframe. 
  # 
  # Returns:
  #   plot: A ggplot plot. 
  plot <- ggplot(sweep, aes(x=t, y=y)) + geom_line()
  plot <- plot + labs(x="Time (s)", y="Amplitude (mV)")
  return(plot)
}
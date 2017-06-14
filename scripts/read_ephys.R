# Taisuke Yasuda
#
# This file implements the useful functions for viewing the ephys data. 

SweepToDataFrame <- function(sweep) {
  # Converts wave data to a data frame. 
  #
  # Args:
  #   sweep: Wave data.
  # 
  # Returns:
  #   df: A data frame containing the the data. 
  ts <- WaveToTimeSeries(sweep)
  return(data.frame(y=as.matrix(ts), t=as.numeric(time(ts))))
}
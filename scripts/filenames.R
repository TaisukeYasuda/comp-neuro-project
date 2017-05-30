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
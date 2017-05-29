# Taisuke Yasuda
#
# This file implements the useful functions for manipulating file names. 

# FileRoot: character -> character
# Parses a filename of the form "root.ext" and returns the root.
FileRoot <- function(filename) {
  substr(filename, 1, nchar(filename) - nchar(tools::file_ext(filename)) - 1)
}
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

IndexedName <- function(name, k) {
  # Concatenates a word with a number. 
  #
  # Args:
  #   name: A word to be appeneded in front of the index k.
  #   k: The index. 
  # 
  # Returns:
  #   indexed.name: Concatenation of the word and the number. 
  return(paste(name, toString(k), sep=""))
}
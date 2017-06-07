# Taisuke Yasuda
#
# This file implements useful functions implementing EM algorithms for
# various models for the postsynaptic potential distribution. 

PopulateIfNull <- function(opts, field, default) {
  # Populates the opts field with a default value if not present
  #
  # Args:
  #   opts: The list to populate the field.
  #   field: The field to populate.
  #   default: The default value to populate when the field is NULL. 
  # 
  # Returns:
  #   opts: The modified list with the field populated. 
  
  if (is.null(opts[field]) || is.na(opts[field])) {
    opts[field] <- default
  }
  return(opts)
}
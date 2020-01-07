# This file contains all the R functions created for this project

# Check the number of unique values in a vector or of unique combination in a 
# matrix
lunique <- function(x, na.remove = T) {
  x <- as.matrix(x)
  ifelse(na.remove, nrow(na.omit(unique(x))), nrow(unique(x)))
}
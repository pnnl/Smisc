# Determines whether all the elements in a row are missing
allMissing <- function(dframe, byRow=TRUE) {

  if ((!is.matrix(dframe)) & (!is.data.frame(dframe)))
    stop("'", deparse(substitute(dframe)),
         "' must be a matrix or a data frame.\n")

  if (byRow)
    margin <- 1
  else
    margin <- 2
  
  f <- function(x) all(is.na(x))

  return(apply(dframe, margin, f))

} # end allMissing()

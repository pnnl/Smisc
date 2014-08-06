# Cumulative sum that doesn't propogate NAs
cumsumNA <- function(x) {

  if (all(is.na(x)))
    return(x)

  n <- length(x)

  # Calculate the cumulative sum
  if (is.integer(x))
    out <- .C("cumsumNAint",
              as.integer(x),
              as.integer(n),
              out = integer(n),
              NAOK = TRUE,
              PACKAGE = "pnlStat")$out
  else
    out <- .C("cumsumNAdouble",
              as.double(x),
              as.integer(n),
              out = double(n),
              NAOK = TRUE,
              PACKAGE = "pnlStat")$out              
    

  # Give appropriate names to out
  if (!is.null(nx <- names(x)))
    names(out) <- nx

  # List the vector with the cumsum
  return(out)
  
} # cumsumNA

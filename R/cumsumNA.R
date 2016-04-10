##' Computes the cummulative sum of a vector without propagating NAs
##'
##' @details If \code{x} is integer, then integer addition is used.  Otherwise, floating
##' point (double) addition is used. Elements in \code{x} that were \code{NA} will continue
##' to be \code{NA}, but the \code{NA} will not be propagated.
##'
##' @export
##' @param x An integer or double vector
##' 
##' @return The vector of cumulative sums.
##'
##' @author Landon Sego
##' 
##' @seealso \code{\link{cumsum}}
##' 
##' @keywords misc
##' 
##' @examples
##' # Compare to cumsum()
##' x <- as.integer(c(5, 2, 7, 9, 0, -1))
##' cumsum(x)
##' cumsumNA(x)
##'
##' # Now with missing values
##' x[c(2,4)] <- NA
##' print(x)
##' cumsum(x)
##' cumsumNA(x)

cumsumNA <- function(x) {

  stopifnotMsg(is.numeric(x), "'x' must be numeric")
    
  if (all(is.na(x)))
    return(x)

  n <- length(x)

  # Calculate the cumulative sum
  if (is.integer(x))
    out <- .C("cumsumNAint",
              as.integer(x),
              as.integer(n),
              out = integer(n),
              NAOK = TRUE)$out
  else
    out <- .C("cumsumNAdouble",
              as.double(x),
              as.integer(n),
              out = double(n),
              NAOK = TRUE)$out

  # Give appropriate names to out
  if (!is.null(nx <- names(x)))
    names(out) <- nx

  # List the vector with the cumsum
  return(out)

} # cumsumNA

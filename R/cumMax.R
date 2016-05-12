##' Computes the maximum of the vector up to the current index
##'
##' For each index in a vector, computes the maximum of the vector from the
##' beginning of the vector up to the current index
##'
##' @export
##' @param x A numeric or integer vector
##'
##' @return In the sequence \emph{x[1], x[2], ..., x[n]}, \code{cumMax}
##' returns the vector \emph{y} such that for each \emph{i = 1,...,n},
##' \emph{y[i] = max(x[j]; j = 1,...,i)}
##'
##' @author Landon Sego
##'
##' @keywords misc
##'
##' @examples
##' cumMax(1:10)
##' cumMax(c(1,3,4,5,3,2,5,1,7,8,8,6))
##' cumMax(c(1,3,4,5,3,2,5,1,7,8,8,6) + runif(12))

cumMax <- function(x) {

  stopifnotMsg(is.numeric(x), "'x' must be 'numeric' or 'integer'")

  n <- length(x)
    
  if (n == 1) {
    return(x)
  }

  else if (is.integer(x)) {
    return(.C("max_le_i_INT",
              as.integer(x),
              as.integer(n),
              y = integer(n))$y)
  }

  else {
    return(.C("max_le_i_DOUBLE",
              as.double(x),
              as.integer(n),
              y = double(n))$y)
  }

} # cumMax

##' Linear mapping of a numerical vector or scalar
##'
##' Linear mapping of a numerical vector or scalar from one contiguous interval
##' to another
##'
##' The mapping is \eqn{f : D \rightarrow R}, where \eqn{f(D[1]) = R[1]} and
##' \eqn{f(D[2]) = R[2]}.
##'
##' @export
##'
##' @param x a numeric vector
##' @param D a vector with 2 elements, the first being the lower endpoint of
##' the domain, the upper being the upper endpoint of the domain. Note
##' \code{R[1]} must be less than \code{R[2]}.
##' @param R a vector with 2 elements indicating the range of the linear
##' mapping. \code{R[1]} is mapped to \code{D[1]}, and \code{R[2]} is mapped to
##' \code{D[2]}.
##' @return The linear mapping of \code{x} from \code{D} to \code{R}
##' @author Landon Sego
##' @examples
##'
##' x <- seq(0, 1, length = 5)
##'
##' # An increasing linear map
##' linearMap(x, R = c(4, 7))
##'
##' # A decreasing map
##' linearMap(x, R = c(7, 4))
##'
##' # A shift
##' linearMap(x, R = c(-1, 0))
##'
##' # The identity map:
##' y <- linearMap(x, D = c(0, 1), R = c(0, 1))
##' identical(y, x)
##'
linearMap <- function(x, D = range(x), R = c(0, 1)) {

  # Check inputs
  stopifnot(is.numeric(x) & is.numeric(D) & is.numeric(R),
            length(D) == 2,
            length(R) == 2,
            D[1] < D[2])

##   # Don't think I want this, because sometimes you may want to map outside of R
##   # Make sure domain of x is contained in D <--
##   rx <- range(x)
##   if ((rx[1] < D[1]) | (rx[2] > D[2]))
##     warning("Unexpected results may occur: The min and max of x [",
##             paste(range(x), collapse = ", "), "] are not in D [",
##             paste(D, collapse = ", "), "]")

  # Return the mapping
  return(R[1] + diff(R) * (x - D[1]) / diff(D))

} # linearMap

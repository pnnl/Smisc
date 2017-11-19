##' Simple numerical integration routine
##'
##' Estimates the integral of a real-valued function using Simpson's or the
##' Trapezoid approximation
##'
##' For the Simpson method, \code{y} is a numeric vector of f(x), evaluated at
##' an odd number of evenly spaced x's in the interval [a,b].
##'
##' For the trapezoid method, the elements of \code{x} and \code{y} should
##' correspond to one another, and \code{x} must be sorted in ascending order.
##' The lengths or \code{x} and \code{y} should be the same, and they may be
##' odd or even. The elements of \code{x} may be irregularly spaced.
##'
##' @export
##' 
##' @param y Vector of f(x) values
##' 
##' @param x Numeric vector of sorted x values, each element of \code{x} should
##' have a corresponding element in \code{y}.  Only required for the trapezoid method.
##' Not required for the Simpson method.
##' 
##' @param a The lower limit of integration, only required for the Simpson
##' method.
##' 
##' @param b The upper limit of integration, only required for the Simpson
##' method.
##' 
##' @param method The method of integration (can use just the first letter).
##' Defaults to \code{simpson}.
##' 
##' @return A single numeric estimate of the integral of f(x) over [a, b]
##' (Simpson) or over the range of \code{x} (trapezoid).
##' 
##' @author Landon Sego
##'
##' @seealso \code{\link{integrate}}
##' 
##' @references Ellis R, Gulick D. "Calculus: One and Several Variables,"
##' Harcourt Brace Jovanovich, Publishers: New York, 1991; 479-482.
##' 
##' @keywords math
##' @examples
##' # The Beta density from 0 to 0.7
##' integ(dbeta(seq(0, 0.7, length = 401), 2, 5), a = 0, b = 0.7)
##'
##' # Checking result with the cdf
##' pbeta(0.7, 2, 5)
##'
##' # f(x) = x^2 from 0 to 3
##' integ(seq(0, 3, length = 21)^2, a = 0, b = 3)
##'
##' # A quadratic function with both methods
##' x <- seq(0, 3, length = 51)
##' integ(x^2, x = x, method = "t")
##' integ(x^2, a = 0, b = 3, method = "s")
##'
##' # Now a linear function
##' x <- seq(0, 2, length = 3)
##' y <- 2 * x + 3
##' integ(y, x = x, method = "t")
##' integ(y, a = 0, b = 2)

integ <- function(y, x = NULL, a = NULL, b = NULL, method = c("simpson", "trapezoid")) {

  # Match args of the methos
  method <- match.arg(method)

  if (method == "simpson") {

    if (!is.null(x))
      warning("'x' is not used in Simpson's method")

    if (is.null(a) | is.null(b))
      stop("Both endpoints 'a' and 'b' are required for Simpson's method")

    # y is a vector of the values of the function, evaluated at an
    # odd number of evenly spaced points in the interval [a,b].
    # a is lower limit, b is upper limit

    # Checks
    if (a > b)
      stop("a must be less than or equal to b\n")

    if ((length(y) + 1) %% 2)
      stop("length(y) must be odd.\n")

    if (length(y) < 3)
      stop("length(y) must be an odd number >= 3\n")

    # Degenerate integral
    if (a == b)
      return(0)

    # Calculate the number of intervals
    n <- length(y) - 1

    # Example of the 'Simpson' weights:
    #
    # If the length of y was 11, the wts vector would be:
    # 1 4 2 4 2 4 2 4 2 4 1
    #
    # If the length of y was 5, the wts vector would be:
    # 1 4 2 4 1
    #
    # If the length of y was 3, the wts vector would be:
    # 1 4 1

    # Generate the Simpson weights
    #  wts <- double(length(y))
    #  for (i in 1:length(y)) {
    #    if (i%%2) wts[i] <- 2
    #    else wts[i] <- 4
    #  }

    # We can generate the weights without the loop
    wts <- ifelse((1:length(y)) %% 2, 2, 4)

    # The first and the last weights are 1
    wts[1] <- wts[length(wts)] <- 1

    # Return the approximation of the integral
    return(as.vector( ((b - a) / (3 * n)) * (t(y) %*% wts) ))

  } # Simpson's method

  # Trapezoid method
  else {

    # Checks
    if (is.null(x))
      stop("'x' is required for the trapezoid method")

    if (!(is.null(a) & is.null(b)))
      warning("Neither 'a' nor 'b' are used with the trapezoid rule")

    if (length(x) != length(y))
      stop("Length's of 'x' and 'y' must be the same")

    if (length(y) < 2)
      stop("'y' must have at least length 2")

    if (!all(diff(x) > 0))
      stop("'x' must be sorted in ascending order and not have duplicate values")

    idx <- 2:length(x)

    return(as.double((x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1]) / 2))

  } # trapezoid method


} # end integ()

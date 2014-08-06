##' Probability functions for the sum of k independent binomials
##'
##' The mass and distribution functions of the sum of k independent binomial
##' random variables, with possibly different probabilities.
##'
##' \code{size[1]} and \code{prob[1]} are the size and probability of the first
##' binomial variate, \code{size[2]} and \code{prob[2]} are the size and
##' probability of the second binomial variate, etc.
##'
##' If the elements of \code{prob} are all the same, then \code{pbinom} or
##' \code{dbinom} is used.  Otherwise, repeating convolutions of the k
##' binomials are used to calculate the mass or the distribution functions.
##'
##' NOTE: When \code{log.p} or \code{log} is \code{TRUE}, these functions do
##' not have the same precision as \code{dbinom} or \code{pbinom} when the
##' probabilities are very small, i.e, the values tend to go to \code{-Inf}
##' more quickly.
##'
##' @aliases dkbinom pkbinom
##' @param q Vector of quantiles (value at which to evaluate the distribution
##' function) of the sum of the k binomial variates
##' @param x Vector of values at which to evaluate the mass function of the sum
##' of the k binomial variates
##' @param size Vector of the number of trials
##' @param prob Vector of the probabilities of success
##' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
##' (See NOTE in details).
##' @param verbose \code{= TRUE} produces output that shows the iterations of
##' the convolutions and 3 arrays, A, B, and C that are used to convolve and
##' reconvolve the distributions.  Array C is the final result.  See the source
##' code in \code{dkbinom.c} for more details.
##' @return \code{dkbinom} gives the mass function, \code{pkbinom} gives the
##' distribution function.  Produces errors for invalid inputs of \code{size},
##' \code{prob}, \code{x}, and \code{q}.
##' @author Landon Sego
##' @seealso \code{\link{dbinom}}, \code{\link{pbinom}}, \code{\link{d2binom}},
##' \code{\link{p2binom}}
##' @references Based on the exact algorithm discussed by
##' Butler, Ken and Stephens, Michael. (1993) The Distribution of a Sum of
##' Binomial Random Variables. Technical Report No. 467, Department of
##' Statistics, Stanford University.
##' @keywords misc
##' @examples
##'
##'  dkbinom(8, c(10, 17), c(0.3, 0.1))
##'  # Compare with p2binom
##'  d2binom(8, 10, 0.3, 17, 0.1)
##'
##'  dkbinom(c(0, 7), c(3, 4, 2), c(0.3, 0.5, 0.8))
##'  pkbinom(c(0, 7), c(3, 4, 2), c(0.3, 0.5, 0.8), verbose = TRUE)
##'

dkbinom <- function(x, size, prob, log = FALSE, verbose = FALSE) {

  x <- check.kbinom(x, size, prob)

  # If only one type of variate was requested or if the probs are equal:
  # If the probs are all equal
  if (all(diff(prob) == 0) | (length(prob) == 1))
    return(dbinom(x, sum(size), prob[1], log = log))

  # Calculate the probabilities
  res <- .C("dkbinom",
            as.integer(max(x)),
            as.integer(size),
            as.double(prob),
            as.integer(length(size)),
            as.integer(FALSE),
            as.integer(verbose),
            double(max(x)+1),
            double(max(x)+1),
            out = double(max(x)+1),
            double(1))[["out"]][x+1]

  if (log)
    res <- log(res)

  return(res)

} # dkbinom


pkbinom <- function(q, size, prob, log.p = FALSE, verbose = FALSE) {

  x <- check.kbinom(q, size, prob)

  # If only one type of variate was requested or if the probs are equal:
  # If the probs are all equal
  if (all(diff(prob) == 0) | (length(prob) == 1))
    return(pbinom(x, sum(size), prob[1], log.p = log.p))

  res <- .C("dkbinom",
            as.integer(max(x)),
            as.integer(size),
            as.double(prob),
            as.integer(length(size)),
            as.integer(FALSE),
            as.integer(verbose),
            double(max(x)+1),
            double(max(x)+1),
            out = double(max(x)+1),
            double(1))[["out"]]

  res <- cumsum(res)[x+1]

  if (log.p)
    res <- log(res)

  return(res)

} # pkbinom


# Function for checking inputs of dkbinom and pkbinom
check.kbinom <- function(x, size, prob) {

  x.name <- deparse(substitute(x))

  # Verify x is an integer
  x.good <- FALSE
  if (is.numeric(x)) {
   if (all(x >= 0))
     x.good <- TRUE
  }
  if (!x.good)
    stop("'", x.name, "' should be a vector of non-negative integers")

  # Change it to an integer if necessary
  if (any(as.logical(x %% 1))) {
    warning("non-integer values of '", x.name,
            "' will be set to integers using 'as.integer()'")
    x <- as.integer(x)
  }

  # size and prob should be the same length
  if (length(size) != length(prob))
    stop("'size' and 'prob' should be the same length")

  # Check that size are postive integers
  size.good <- FALSE
  if (is.numeric(size)) {
    if (all(size %% 1 == 0) & all(size > 0))
      size.good <- TRUE
  }
  if (!size.good)
    stop("'size' must be a vector of positive integers")

  # Check on probabilities
  prob.good <- FALSE
  if (is.numeric(prob)) {
    if (all(prob >= 0) & all(prob <= 1))
      prob.good <- TRUE
  }
  if (!prob.good)
    stop("'prob' must be a vector of numeric values in [0, 1]")

  return(x)

} # check.kbinom

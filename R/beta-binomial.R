##' The Beta-Binomial Distribution
##'
##' Density, distribution function, quantile function, and random generation
##' for the beta-binomial distribution.  A variable with a beta-binomial
##' distribution is distributed as binomial distribution with parameters
##' \code{N} and \code{p}, where the probability \code{p} of success itself has
##' a beta distribution with parameters \code{u} and \code{v}.
##'
##' The beta-binomial distribution with parameters \eqn{N}, \eqn{u}, and
##' \eqn{v} has density given by \deqn{ choose(N, x) * Beta(x + u, N - x + v) /
##' Beta(u,v) } for \eqn{u > 0}, \eqn{v > 0}, a positive integer \eqn{N}, and
##' any nonnegative integer \eqn{x}. Although one can express the integral in
##' closed form using generalized hypergeometric functions, the implementation
##' of distribution function used here simply relies on the cumulative sum of
##' the density.
##'
##' The mean and variance of the beta-binomial distribution can be computed
##' explicitly as \deqn{ mu = \frac{nu}/{u+v} } and \deqn{ sigma^2 =
##' \frac{nuv(n+u+v)}{(u+v)^2 (1+u+v)} }
##'
##' @aliases dbb pbb qbb rbb
##'
##' @export dbb
##' @export pbb
##' @export qbb
##' @export rbb
##'
##' @usage
##' dbb(x, N, u, v)
##' pbb(q, N, u, v)
##' qbb(p, N, u, v)
##' rbb(n, N, u, v)
##'
##' @param x vector of qauntiles
##' @param q vector of quantiles
##' @param p vector of probabilities
##' @param n number of observations
##' @param N number of trials (a positive integer)
##' @param u first positive parameter of the beta distribution
##' @param v second positive parameter of the beta distribution
##' @return \code{dbb} gives the density, \code{pbb} gives the distribution
##' function, \code{qbb} gives the quantile function, and \code{rbb} generates
##' random deviates.
##' @note \code{dbb} and \code{qbb} do not recycle vector-valued arguments in
##' the usual fashion in R.  Arguments to these functions must have length 1
##' and/or one other length. See example of \code{\link{pcbinom}} for more
##' information.
##'
##' Non integer values of \code{x} and \code{N} are rounded to the nearest
##' integer with a warning.
##' @section Warning: \code{dbb}, \code{pbb}, and \code{qbb} can be imprecise
##' when the probabilities for tail values are small.  This can result in
##' unusual behavior for \code{pbb} and \code{qbb}.
##' @author Kevin R. Coombes, modified by Landon Sego
##' @seealso \code{\link{dbeta}} for the beta distribution and
##' \code{\link{dbinom}} for the binomial distribution.
##' @keywords misc
##' @examples
##'
##' # set up parameters
##' w <- 10
##' u <- 0.3*w
##' v <- 0.7*w
##' N <- 12
##'
##' # generate random values from the beta-binomial
##' x <- rbb(1000, N, u, v)
##'
##' # check that the empirical summary matches the theoretical one
##' summary(x)
##' qbb(c(0.25, 0.50, 0.75), N, u, v)
##'
##' # check that the empirical histogram matches the theoretical density
##' hist(x, breaks=seq(-0.5, N + 0.55), prob=TRUE)
##' lines(0:N, dbb(0:N, N, u,v), type='b')
##'
##' # An example of the imprecision in pbb and qbb:  we would expect
##' # this to return 0:29
##' qbb(pbb(0:29, 29, 1, 33), 29, 1, 33)
##'

dbb <- function(x, N, u, v) {

  # Convert x and N to integers with warning
  x.r <- round(x, 14)
  if (any((x.r %% 1) != 0))
    warning("Values of x were rounded to nearest integer\n")
  N.r <- round(N, 14)
  if (any((N.r %% 1) != 0))
    warning("Values of N were rounded to nearest integer\n")

  x <- as.integer(round(x))
  N <- as.integer(round(N))

  # Verify arguments are legitimate
  if (any(u <= 0))
    stop("u must be > 0")
  if (any(v <= 0))
    stop("v must be > 0")
  if (any(x < 0))
    stop("x must be >= 0")
  if (any(N < 0))
    stop("N must be >= 0")
  if (any(x > N))
    stop("x must be <= N")

  #beta(x+u, N-x+v)/beta(u,v)*choose(N,x)
  choose(N, x) * exp(lgamma(x + u) + lgamma(N - x + v) - lgamma(N + u + v) + lgamma(u + v) - lgamma(u) - lgamma(v))

} # dbb

pbb <- function(q, N, u, v) {

  # Checking and adjusting N
  N.r <- round(N, 14)
  if (any((N.r %% 1) != 0))
    warning("Values of N were rounded to nearest integer\n")

  N <- as.integer(round(N))

  # u, and v will get checked in dbb


  q <- floor(q)

  # Make a vector of the lengths of each argument
  l.q <- length(q)
  l.N <- length(N)
  l.u <- length(u)
  l.v <- length(v)

  # This function does not recycle vectors in the usual fashion.  It expects lengths of arguments to be 1 and/or one other number
  # The following tests this condition

  # Gather the lengths of each argument that length > 1 into a single vector
  lens <- c(l.q, l.N, l.u, l.v)
  lens.gt.1 <- lens[lens > 1]

  # If there are any arguments with length > 1
  if (length(lens.gt.1)) {
    # If there are more than 1 argument with length > 1
    if (length(lens.gt.1) > 1) {
      # If those args that have length > 1 are not the same length
      if (!all(diff(lens.gt.1) == 0))
        stop("'pbb' does not recycle vectors in the usual fashion.\n",
             "It expects the lengths of arguments to be 1 and/or one other number.\n")
    }
  }

  # Find the longest argument
  l.vec <- max(lens)

  # Make vectors all same length
  if (l.q == 1)
    q <- rep(q, l.vec)
  if (l.N == 1)
    N <- rep(N, l.vec)
  if (l.u == 1)
    u <- rep(u, l.vec)
  if (l.v == 1)
    v <- rep(v, l.vec)

  out <- double(l.vec)

  # Calculate the results separately for each instance
  for (i in 1:l.vec) {
    if (q[i] < 0)
      out[i] <- 0
    else if (q[i] >= N[i])
      out[i] <- 1
    else
      out[i] <- sum(dbb(0:q[i], N[i], u[i], v[i]))
  }


  # Fix any values that exceed 0 or 1 due to machine error
  out[out > 1] <- 1
  out[out < 0] <- 0

  return(out)

} # pbb


qbb <- function(p, N, u, v) {

  # Checking and adjusting N
  N.r <- round(N, 14)
  if (any((N.r %% 1) != 0))
    warning("Values of N were rounded to nearest integer\n")

  N <- as.integer(round(N))

  # u, and v will get checked in dbb

  # Check values of p
  if (any(p > 1) | any(p < 0))
    stop("p must be in [0,1]")

  # Make a vector of the lengths of each argument
  l.p <- length(p)
  l.N <- length(N)
  l.u <- length(u)
  l.v <- length(v)

  # This function does not recycle vectors in the usual fashion.  It expects lengths of arguments to be 1 and/or one other number
  # The following tests this condition


  # Gather the lengths of each argument that length > 1 into a single vector
  lens <- c(l.p, l.N, l.u, l.v)
  lens.gt.1 <- lens[lens > 1]

  # If there are any arguments with length > 1
  if (length(lens.gt.1)) {
    # If there are more than 1 argument with length > 1
    if (length(lens.gt.1) > 1) {
      # If those args that have length > 1 are not the same length
      if (!all(diff(lens.gt.1) == 0))
        stop("'qbb' does not recycle vectors in the usual fashion.\n",
             "It expects the lengths of arguments to be 1 and/or one other number.\n")
    }
  }

  # Find the longest argument
  l.vec <- max(lens)

  # Make vectors all same length
  if (l.p == 1)
    p <- rep(p, l.vec)
  if (l.N == 1)
    N <- rep(N, l.vec)
  if (l.u == 1)
    u <- rep(u, l.vec)
  if (l.v == 1)
    v <- rep(v, l.vec)

  out <- double(l.vec)

  # Calculate the results separately for each instance
  for (i in 1:l.vec) {
    if (p[i] == 1)
      out[i] <- N[i]
    else {
      pp <- cumsum(dbb(0:N[i], N[i], u[i], v[i]))
      out[i] <- sum(pp < p[i])
    }
  }

  return(out)

} # qbb

# I Assume these recycle properly?...
rbb <- function(n, N, u, v) {

  # Verify arguments are correct
  if (any(u <= 0))
    stop("u must be > 0")
  if (any(v <= 0))
    stop("v must be > 0")

  N.r <- round(N, 14)
  if (any((N.r %% 1) != 0))
    warning("Values of N rounded to nearest integer\n")

  N <- as.integer(round(N))

  p <- rbeta(n, u, v)
  rbinom(n, N, p)

} # rbb



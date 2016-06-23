##' Calculate the highest posterior density credible interval for a unimodal density
##'
##' @details
##' Parallel processing (via \code{njobs > 1}) may be advantageous if 1) \code{pdf} is a function that is computationally expensive, 2)
##' the \code{cdf} is not provided, in which case \code{pdf} is integrated, and/or 3) when \code{checkUnimodal} is large.
##'
##' @export
##' @rdname hpd
##'
##' @param pdf Function that takes a single numeric vector argument that returns a vector
##' of probability density values
##'
##' @param support A numeric vector of length 2 giving the interval over which the random variable has
##' support (i.e. for which the pdf is positive).  For now, this must be a finite interval.
##' Intervals for random variables within infinite support
##' can still be calculated by setting the values of support to be suitably large and/or small.  See examples.
##'
##' @param prob A numeric value in (0, 1] indicating the size of the desired probability for the credible
##' interval.
##'
##' @param cdf A function that takes a single (not necessarily vector) argument and returns the cumulative probability.
##' If \code{NULL}, the pdf is integrated as needed to calculate probabilities
##' as needed.  However, providing the \code{cdf} will speed up calculations.
##'
##' @param njobs The number of parallel jobs to spawn (where possible) using \code{\link{doCallParallel}}.  This is helpful if \code{pdf} is
##' expensive.
##' 
##' @param checkUnimodal An integer that, when greater than 0, indicates the number of points in \code{support} for which \code{pdf} is
##' evaluated to determine whether the function appears unimodal. This is done in parallel if \code{njobs > 1}.
##' If \code{checkUnimodal} is not 0, it should be a large number (like 1000 or more). 
##'
##@param invcdf A function that takes a single argument in [0, 1] and returns the inverse of the
## cumulative distribution function. This is required if \code{support} is not finite.
##'
##' @return A list of class \code{hpd} that contains the following elements:
##' \describe{
##' \item{lower}{The lower endpoint of the highest posterior density interval}
##' \item{upper}{The lower endpoint of the highest posterior density interval}
##' \item{prob}{The acheived probability of the interval}
##' \item{cut}{The horizontal cut point that gave rise to the interval}
##' \item{mode}{The mode of the density}
##' \item{pdf}{The probability density function}
##' \item{support}{The support of the pdf}
##' }
##'
##' @author Landon Sego
##'
##' @examples
##' \donttest{
##' # A credible interval using the standard normal
##' int <- hpd(dnorm, c(-5,5), prob = 0.90, njobs = 2)
##' print(int)
##' plot(int)
##' }
##' 
##' # A credible interval with the gamma density
##' int <- hpd(function(x) dgamma(x, shape = 2, rate = 0.5), c(0, 20),
##'            cdf = function(x) pgamma(x, shape = 2, rate = 0.5), prob = 0.8)
##' print(int)
##' plot(int)
##'
##' \donttest{
##' # A credible interval using the Beta density
##' dens <- function(x) dbeta(x, 7, 12)
##' dist <- function(x) pbeta(x, 7, 12)
##' int <- hpd(dens, c(0, 1), cdf = dist)
##' print(int)
##' plot(int)
##' }

hpd <- function(pdf, support, prob = 0.95, cdf = NULL, njobs = 1, checkUnimodal = 0) { #, invcdf = NULL) {

  # Basic checks
  stopifnotMsg(# pdf
               is.function(pdf),
               "'pdf' must be a function",
               # support
               if (is.numeric(support) & length(support) == 2) {
                 support[1] < support[2]
               } else FALSE,
               "'support' must be a numeric vector of length 2, with the first element less than the second",
               # cdf
               if (!is.null(cdf)) is.function(cdf) else TRUE,
               "'cdf' must be NULL or a function",
               # prob
               if (is.numeric(prob) & length(prob) == 1) {
                 (prob > 0) & (prob <= 1)
               } else FALSE,
               "'prob' must be a single numeric value in (0, 1]",
               # njobs
               if (is.numeric(njobs) & length(njobs) == 1) {
                 (njobs >= 1) & (njobs %% 1 == 0)
               } else FALSE,
               "'njobs' must be a single value in {1, 2, 3, ...}",
               # checkUnimodal
               if (is.numeric(checkUnimodal) & length(checkUnimodal) == 1) {
                 (checkUnimodal >= 0) & (checkUnimodal %% 1 == 0)
               } else FALSE,
               "'checkUnimodal' must be a single value in {0, 1, 2, ...}")

  # Verify pdf is unimodal
  if (checkUnimodal > 0) {

    # Define a sequence over which to check the function
    xseq <- seq(support[1], support[2], length = checkUnimodal)

    # Evaluate the function across xseq
    yseq <- doCallParallel(pdf, xseq, njobs = njobs, random.seed = rpois(1, 1000))

    # Look for changes in sign in the local derivative
    fdiff <- sign(diff(yseq))

    # Remove flat spots
    if (any(fdiff == 0)) {
      fdiff <- fdiff[-which(fdiff == 0)]
    }

    # Count the number of changes in the sign of the derivative
    numChanges <- sum(diff(fdiff) != 0)

    if ((numChanges > 1) | (fdiff[100] < 0) | (fdiff[900] > 0)) {
      warning("'pdf' may not be unimodal, in which case the resulting credible interval may not be the shortest possible")
    }

  }

  # If the support is infinite on one or more of the bounds, find a finite values way out in the tail where
  # the value is close to 0
  ## if (any(abs(support) == Inf)) {

  ##   if (is.null(invcdf))
  ##     stop("When 'support' is not finite, 'invcdf' must be provided")

  ##   # Replace the lower and upper values of the support with something large in magnitude but finite

  ##   # If the lower value is -Inf
  ##   if (support[1] == -Inf)
  ##     support[1] <- invcdf(1e-12)

  ##   # If the upper value if Inf
  ##   if (support[2] == Inf)
  ##     support[2] <- invcdf(1 - 1e-12)

  ## } # If the support is not finite

  # Find the mode
  peak <- optimize(pdf, lower = support[1], upper = support[2],
                   maximum = TRUE)$maximum

  # Attempt to calculate probability via integrating the pdf if cdf not provided
  if (is.null(cdf)) {

    pdfInterval <- function(lower, upper) {

      # Do the calls in parallel
      pdfParallel <- function(x) {
        doCallParallel(pdf, x, njobs = njobs, random.seed = rpois(1, 1000))
      }

      return(integrate(pdfParallel, lower = max(lower, support[1]), upper = min(upper, support[2]))$value)

    } # pdfInterval

  } # If is.null(cdf)

  # For a given horizontal cut of the density, calculate the area under the curve
  area <- function(cut) {

      # Get the two endpoints in the support that correspond to the cut
      objFun <- function(x) {
#        pvar(x, pdf(x), cut, sign(pdf(x) - cut))
         pdf(x) - cut
      }

      # To the left of the mode
      v1 <- uniroot(objFun, interval = c(support[1], peak), extendInt = "upX")$root
#      pvar(v1, support[1], peak)

      # To the right of the mode
      v2 <- uniroot(objFun, interval = c(peak, support[2]), extendInt = "downX")$root
#      pvar(v2, support[2], peak)

      # Find area under the curve
      if (is.null(cdf))
        achievedProb <- pdfInterval(v1, v2)
      else
        achievedProb <- cdf(v2) - cdf(v1)

      # Now the area under the curve
      return(list(lower = v1,
                  upper = v2,
                  prob = achievedProb))

  } # area

  # Find the larger of the endpoints as a starting place for the cuts
  loCut <- max(pdf(c(support[1], support[2])))

  # Find the higest cut we'll want to consider
  hiCut <- pdf(peak) - 1e-10

  # Now solve for the cut that gives the desired probability
  cutSolution <- uniroot(function(y) area(y)$prob - prob, interval = c(loCut, hiCut))$root

  # Now return the interval and the mode
  out <- c(area(cutSolution), list(cut = cutSolution, mode = peak, pdf = pdf, support = support))
  class(out) <- c("hpd", class(out))

  return(out)

} # hpd

################################################################################
# Printing and plotting methods
################################################################################

##' @method print hpd
##'
##' @describeIn hpd Prints the lower and upper limits of the credible interval, along with the achieved
##' probabilty of that interval.
##'
##' @param  x object of class \code{hpd}, returned by \code{hpd}
##'
##' @export
print.hpd <- function(x, ...) {

  print(x[c("lower", "upper", "prob")])

} # print.hpd

##' @method plot hpd
##'
##' @describeIn hpd Plots the density, overlaying the lower and upper limits of the credible interval
##'
##' @param \dots For the \code{plot} method, these are additional arguments that may be passed to
##' \code{\link{plotFun}}, \code{\link{plot.default}}, or \code{\link{abline}}
##'
##' @export

plot.hpd <- function(x, ...) {

  # Set the default plotting args
  args <- list(fun = x$pdf, xlim = x$support,
               xlab = expression(x), ylab = expression(f(x)), col = "Red")

  # Default value for ablineArgs
  ablineArgs1 <- list()
  ablineArgs2 <- list(lty = 3)

  # Supplied args
  supArgs <- list(...)

  # Add the supplied args, ommiting duplicates in the default args
  if (length(supArgs))  {

    # Args for plotFun
    args <- c(supArgs, args[setdiff(names(args), names(supArgs))])

    # Select only graphical args before passing to abline()
    ablineArgs1 <- supArgs[names(supArgs) %in% names(par())]

    # Select only graphical args before passing to abline(), but retain control
    # of lty
    ablineArgs2 <- c(list(lty = 3), supArgs[names(supArgs) %in% setdiff(names(par()), "lty")])

  }

  # Make the plot
  do.call(plotFun, args)

  # Add in the lines
  do.call(abline, c(list(v = c(x$lower, x$upper)), ablineArgs1))
  do.call(abline, c(list(h = x$cut), ablineArgs2))

} # plot.hpd

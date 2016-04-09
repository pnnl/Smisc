##' Call a function with a vectorized input in parallel
##'
##' Call a function with a vectorized input in parallel, where the function is computationally intensive.
##'
##' This function is a parallelized wrapper for \code{\link{do.call}} designed for the case where \code{fun} is
##' computationally intensive.  Each element of \code{x} is evaluated
##' independently of the other elements of \code{x}.  Thus, \code{fun(c(x1,x2))} must be equivalent
##' to \code{c(fun(x1), fun(x2))} in order for \code{doCallParallel} to work properly.
##'
##' @export
##' @param fun A function, or a text string with the name of the function, whose first argument is a vector and
##' returns a corresponding vector
##'
##' @param x A vector of values that is the first argument to \code{fun}
##'
##' @param njobs The number of parallel jobs to spawn using \code{\link{parLapplyW}}.
##'
##' @param random.seed If a numeric value is provided, \code{x} is randomized to better distribute the work among
##' the jobs if some values of \code{x} take longer to evaluate than others.
##' The original ordering is restored before \code{fun(x, ...)} is returned. If \code{NULL},
##' no randomization is performed.
##'
##' @param \dots Additional named arguments for \code{fun}
##'
##' @return The same result that would be had by calling \code{fun(x, ...)}, except calculated in parallel
##'
##' @author Landon Sego
##'
##' @examples
##' # Get a vector of x's
##' x <- rnorm(18, mean = 2, sd = 2)
##'
##' # 2 cores
##' y1 <- doCallParallel("pnorm", x, mean = 2, sd = 2, njobs = 2)
##'
##' # 2 cores and randomization
##' y2 <- doCallParallel(pnorm, x, mean = 2, sd = 2, njobs = 2, random.seed = 1)
##'
##' # Without using doCallParallel()
##' y3 <- pnorm(x, mean = 2, sd = 2)
##'
##' # Comparisons
##' identical(y1, y2)
##' identical(y1, y3)

doCallParallel <- function(fun, x, ..., njobs = parallel::detectCores() - 1, random.seed = NULL) {

  # Argument checks
  stopifnotMsg(if (is.character(fun)) is.function(get(fun)) else is.function(fun),
               "'fun' must be a function or the character string of the name of the function",
               is.vector(x), "'x' must be a vector",
               is.numeric(njobs) & (njobs >= 1), "'njobs' must be a positive numeric (integer) value",
               length(njobs) == 1, "'njobs' must have length 1",
               if (!is.null(random.seed)) is.numeric(random.seed) else TRUE,
               "'random.seed' must be numeric or NULL")

  # Make sure the number of jobs is no larger than the length of x, and no smaller than 1
  njobs <- as.integer(max(1, min(length(x), njobs)))

  # If only 1 job
  if (njobs == 1) {

    return(do.call(fun, list(x, ...)))

  # To parallelize
  } else {

    # Create the job ordering
    xparse <- parseJob(length(x), njobs = njobs, random.seed = random.seed)

    # Get the extra arguments
    args <- list(...)

    # Function for parLapply
    doCall <- function(subset) {
      do.call(fun, c(list(x[subset]), args))
    }

    # Run the calculation in parallel
    out <- unlist(parLapplyW(xparse, doCall, njobs = njobs, varlist = c("fun", "x", "args")))

    # Reorder if needed
    if (is.null(random.seed))
      return(out)
    else
      return(out[order(unlist(xparse))])

  }

} # doCallParallel

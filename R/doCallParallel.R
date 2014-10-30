##' Call a function with a vectorized input in parallel
##'
##' Call a function with a vectorized input in parallel
##'
##' This function is designed for the case where \code{fun} is computationally intensive.  Each element of \code{x} is evaluated
##' independently of the other elements of \code{x}.  Thus, we require that \code{fun(c(x1,x2))} be equivalent
##' to \code{c(fun(x1), fun(x2))}.
##'
##' @export
##' 
##' @param fun A function that whose first argument is a vector and returns a corresponding vector
##' 
##' @param x A vector of values that is the first argument to the function
##' 
##' @param nJobs The number of parallel jobs to spawn using \code{\link{mclapply}}. Note that \code{nJobs > 1} only works
##' for non-Windows machines
##' 
##' @param \dots Additional named arguments for \code{fun}
##'
##' @return The same result that would be had by calling \code{fun(x, ...)}, except calculated in parallel
##'
##' @examples 
##' # Get a vector of x's
##' x <- rnorm(18, mean = 2, sd = 2)
##' 
##' # 2 cores
##' y1 <- doCallParallel(pnorm, x, mean = 2, sd = 2, nJobs = 2)
##' 
##' # 1 core
##' y2 <- doCallParallel(pnorm, x, mean = 2, sd = 2, nJobs = 1)
##' 
##' # Without using doCallParallel()
##' y3 <- pnorm(x, mean = 2, sd = 2)
##' 
##' # Comparisons
##' identical(y1, y2)
##' identical(y1, y3)

doCallParallel <- function(fun, x, nJobs = parallel::detectCores(), ...) {

  # Argument checks
  stopifnot(is.function(fun),
            is.vector(x),
            is.numeric(nJobs),
            length(nJobs) == 1)

  # Make sure the number of jobs is no larger than the vector x
  nJobs <- min(length(x), nJobs)

  # If only 1 job or windows
  if ((nJobs == 1) | (.Platform$OS.type == "windows")) {
      
    return(do.call(fun, list(x, ...)))

  # To parallelize 
  } else {

    return(unlist(parallel::mclapply(parseJob(length(x), njobs = nJobs),
                                     function(subset) do.call(fun, list(x[subset], ...)),
                                     mc.cores = nJobs)))
  }
  
} # doCallParallel



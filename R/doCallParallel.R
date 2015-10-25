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
##' 
##' @param fun A function, or a text string with the name of the function, whose first argument is a vector and
##' returns a corresponding vector
##' 
##' @param x A vector of values that is the first argument to the function
##' 
##' @param nJobs The number of parallel jobs to spawn using \code{\link{parLapply}}.
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
##' y1 <- doCallParallel("pnorm", x, mean = 2, sd = 2, nJobs = 2)
##'
##' # 2 cores and randomization
##' y2 <- doCallParallel(pnorm, x, mean = 2, sd = 2, nJobs = 2, random.seed = 1)
##' 
##' # 1 core
##' y3 <- doCallParallel(pnorm, x, mean = 2, sd = 2, nJobs = 1)
##' 
##' # Without using doCallParallel()
##' y4 <- pnorm(x, mean = 2, sd = 2)
##' 
##' # Comparisons
##' identical(y1, y2)
##' identical(y1, y3)
##' identical(y1, y4)

doCallParallel <- function(fun, x, nJobs = parallel::detectCores(), random.seed = NULL, ...) {

  # Argument checks
  stopifnot(if (is.character(fun)) is.function(get(fun)) else is.function(fun),
            is.vector(x),
            is.numeric(nJobs),
            length(nJobs) == 1)

  # Make sure the number of jobs is no larger than the vector x
  nJobs <- min(length(x), nJobs)

  # If only 1 job
  if (nJobs == 1) {
      
    return(do.call(fun, list(x, ...)))

  # To parallelize 
  } else {

    # Create the job ordering
    xparse <- parseJob(length(x), njobs = nJobs, random.seed = random.seed)

    # Get the extra arguments
    args <- list(...)

    # Function for parLapply
    doCall <- function(subset) {
      do.call(fun, c(list(x[subset]), args))
    }
    
    # Start the cluster
    cl <- parallel::makeCluster(nJobs)

    # Send needed objects to cluster
    parallel::clusterExport(cl, c("fun", "x", "args"), envir = environment())

    # Run the calculation
    out <- unlist(parallel::parLapply(cl, xparse, doCall))

    # Shut down the cluster
    parallel::stopCluster(cl)
                  
    # Reorder if needed
    if (is.null(random.seed))
      return(out)
    else
      return(out[order(unlist(xparse))])
  
  }
  
} # doCallParallel

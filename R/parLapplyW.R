##' A wrapper for parLapply
##'
##' A wrapper to make calls to \code{\link{parLapply}} easier by initializing the cluster, exporting objects and expressions to the
##' worker nodes, and shutting down the cluster.
##'
##' The expression in \code{expr} is evaluated before the variables in \code{varlist} are exported.
##'
##' @export
##' @param X  A vector (atomic or list)
##'
##' @param FUN A function or character string naming a function whose first argument will be passed the elements of \code{X}
##'
##' @param \dots Additional named arguments to \code{FUN}
##'
##' @param njobs The number of jobs (cores) to use
##'
##' @param expr An expression that will be evaluated on each worker node via a call to \code{\link{clusterEvalQ}}
##'
##' @param varlist Character vector of names of objects to export to each worker node via \code{\link{clusterExport}}
##'
##' @param envir The environment containing the variables in \code{varlist} that will be exported
##'
##' @return The same result given by \code{lapply(X, FUN, ...)}
##'
##' @seealso \code{\link{lapply}}, \code{\link{parLapply}}, \code{\link{plapply}}
##'
##' @author Landon Sego
##'
##' @examples
##'# Create a simple list
##'a <- list(a = rnorm(10), b = rnorm(20), c = rnorm(15))
##'
##'# Some objects that will be needed by f1:
##'b1 <- rexp(20)
##'b2 <- rpois(10, 20)
##'
##'# The function, which will depend on the Smisc package
##'f1 <- function(x, someText = "this.stuff") {
##'  textJunk <- stripExtension(someText)
##'  result <- mean(x) + max(b1) - min(b2)
##'  return(list(textJunk, result))
##'}
##'
##'# Call parLapplyW(), loading the Smisc package and passing in the "b1" and "b2" objects
##'res.1 <- parLapplyW(a, f1, someText = "that.stuff", njobs = 2,
##'                    expr = expression(library(Smisc)),
##'                    varlist = c("b1", "b2"))
##'
##'print(res.1)
##'
##'# Call parLapplyW(), note that we're sending a different value for "b2" into the worker nodes
##'# via the 'expr' argument
##'res.2 <- parLapplyW(a, f1, someText = "that.stuff", njobs = 2,
##'                    expr = expression({library(Smisc); b2 <- rnorm(10)}),
##'                    varlist = c("b1"))
##'
##'# These should not be equivalent
##'identical(res.1, res.2)
##'
##'# Call lapply
##'res.3 <- lapply(a, f1, someText = "that.stuff")
##'
##'# Compare results, these should be equivalent
##'identical(res.1, res.3)
##'

parLapplyW <- function(X, FUN, ..., njobs = parallel::detectCores() - 1,
                       expr = NULL, varlist = NULL, envir = parent.frame()) {

  if (njobs < 1) {
    njobs <- 1
  }

  # Start the cluster
  cl <- parallel::makeCluster(njobs)

  # Create a protected environment for try-catching errors to ensure
  # we get the cluster shut down if errors occur
  inner <- function() {

    # Evaluate expression on the cluster
    if (!is.null(expr)) {

      parallel::clusterCall(cl, eval, expr, env = .GlobalEnv)

    }

    # Export the variables
    if (!is.null(varlist)) {

      parallel::clusterExport(cl, varlist, envir = envir)

    }

    # Run the parallel lapply
    return(parallel::parLapply(cl, X, FUN, ...))

  } # inner

  # Call with the try-catch
  out <- try(inner(), silent = TRUE)

  # Shut down the cluster
  parallel::stopCluster(cl)

  # Return the result depending on the error status
  if (class(out) == "try-error")
    stop(out)
  else
    return(out)

} # parLapplyW

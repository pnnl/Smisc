##' Parallel wrapper for plyr::ddply
##'
##' Parallel implementation of \code{\link[plyr:ddply]{plyr::ddply}} that suppresses a spurious warning when
##' \code{\link[plyr:ddply]{plyr::ddply}} is called in parallel.
##' All of the arguments except \code{njobs} are passed directly to arguments of the same name in
##' \code{\link[plyr:ddply]{plyr::ddply}}.
##'
##' An innocuous warning is thrown when \code{\link[plyr:ddply]{plyr::ddply}} is called in parallel:
##' \url{https://github.com/hadley/plyr/issues/203}.  This function catches and hides that warning, which looks like this:
##' \verb{
##'
##' Warning messages:
##' 1: <anonymous>: ... may be used in an incorrect context: '.fun(piece, ...)'
##' }
##'
##' If \code{njobs = 1}, a call to \code{\link[plyr:ddply]{plyr::ddply}} is made without parallelization, and anything
##' supplied to \code{.paropts} is ignored. See the documentation for \code{\link[plyr:ddply]{plyr::ddply}} for additional details.
##'
##' @export
##' @param .data data frame to be processed
##'
##' @param .variables character vector of variables in \code{.data} that will define how to split the data
##'
##' @param .fun function to apply to each piece
##'
##' @param \dots other arguments passed on to '.fun'
##'
##' @param njobs the number of parallel jobs to launch, defaulting to one less than the number of available cores on the machine
##'
##' @param .progress name of the progress bar to use, see \code{\link[plyr:create_progress_bar]{plyr::create_progress_bar}}
##'
##' @param .inform produce informative error messages?  This is turned off by default because it substantially
##' slows processing speed, but is very useful for debugging
##'
##' @param .drop should combinations of variables that do not appear in the input data be
##' preserved (FALSE) or dropped (TRUE, default)
##'
##' @param .paropts a list of additional options passed into the \code{\link[foreach:foreach]{foreach::foreach}} function
##' when parallel computation is enabled.  This is important if (for example) your code relies on external data
##' or packages. Use the \code{.export} and \code{.packages} arguments to
##' supply them so that all cluster nodes have the correct environment set up for computing.
##'
##' @return  The object data frame returned by \code{\link[plyr:ddply]{plyr::ddply}}
##'
##' @seealso \code{\link[plyr:ddply]{plyr::ddply}}
##'
##' @examples
##'data(baseball, package = "plyr")
##' 
##'\donttest{
##'# Summarize the number of entries for each year in the baseball dataset with 2 jobs
##'o1 <- pddply(baseball, ~ year, nrow, njobs = 2)
##'head(o1)
##'
##'#  Verify it's the same as the non-parallel version of plyr::ddply()
##'o2 <- plyr::ddply(baseball, ~ year, nrow)
##'identical(o1, o2)
##'
##' 
##'# Another possibility
##'o3 <- pddply(baseball, "lg", c("nrow", "ncol"), njobs = 2)
##'o3
##'
##'o4 <- plyr::ddply(baseball, "lg", c("nrow", "ncol"))
##'identical(o3, o4)
##'}
##' 
##'# A nonsense example where we need to pass objects and packages into the cluster
##'number1 <- 7
##'
##'f <- function(x, number2 = 10) {
##'  paste(x$id[1], padZero(number1, num = 2), number2, sep = "-")
##'}
##'
##'# In parallel
##'o5 <- pddply(baseball[1:100,], "year", f, number2 = 13, njobs = 2,
##'             .paropts = list(.packages = "Smisc", .export = "number1"))
##'o5
##'
##'\donttest{
##'# Non parallel
##'o6 <- plyr::ddply(baseball[1:100,], "year", f, number2 = 13)
##'identical(o5, o6)
##'}

pddply <- function(.data, .variables, .fun = NULL, ...,
                   njobs = parallel::detectCores() - 1, .progress = "none",
                   .inform = FALSE, .drop = TRUE, .paropts = NULL) {

    # Sanity check
    if (njobs < 1) {
      njobs <- 1
    }

    # Non parallel option
    if (njobs == 1) {
      return(plyr::ddply(.data, .variables, .fun = .fun, ...,
                         .progress = .progress, .inform = .inform,
                         .drop = .drop, .parallel = FALSE))
    }

    # Make sure the foreach package is available
    if (!requireNamespace("foreach", quietly = TRUE)) {
      stop("The 'foreach' package is required by pddply() when 'njobs > 1'")
    }

    # Set up the cluster
    cl <- parallel::makeCluster(njobs)
    doParallel::registerDoParallel(cl)

    # Process the results
    o <- tryCatchWE(plyr::ddply(.data, .variables, .fun = .fun, ...,
                                .progress = .progress, .inform = .inform,
                                .drop = .drop, .parallel = TRUE, .paropts = .paropts))

    # Shut down the cluster
    parallel::stopCluster(cl)

    # If we have an error, then stop
    if (is(o$value, "error")) {
      stop(o$value)
    }

    # Extract and remove bogus warnings from the plyr package
    if (!is.null(o$warning)) {

      # Seemingly unique strings from the bogus warning
      bogus1 <- c("<anonymous>: ... may be used in an incorrect context:")
      bogus2 <- c(".fun(piece, ...)")

      # Determine whether these strings are present in any warnings
      vWarn <- function(x) {
        return(!(grepl(bogus1, x, fixed = TRUE) & grepl(bogus2, x, fixed = TRUE)))
      }

      # Identify the valid warnings
      validWarnings <- unlist(lapply(o$warning, vWarn))

      # I don't think this portion of the code will ever be really used--
      # because as far as I can tell, both ddply() (and parLapply(), for that
      # matter) don't capture and return warnings from the worker nodes.
      # But if there any other warnings, issue them
      if (any(validWarnings)) {

        nothing <- lapply(o$warning[validWarnings], warning)

      }

    }

    # Return the result
    return(o$value)

} # pddply


# I got this idea from demo(error.catching).  This actually trapped the warnings,
# whereas suppressWarnings() made them vanish completely
tryCatchWE <- function(expr) {

  W <- NULL

  w.handler <- function(w) {
 	  W <<- c(W, w = list(w))
  	invokeRestart("muffleWarning")
  }

  return(list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
 			                                    warning = w.handler),
              warning = W))

} # tryCatchWE

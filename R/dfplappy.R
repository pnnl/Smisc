##' Parallelized single row processing of a data frame
##' 
##' Applies a function to each row of a data frame in a parallelized fashion
##' (by submitting multiple batch R jobs)
##' 
##' \code{dfplapply} is a convenient wrapper for \code{plapply}, modified
##' especially for parallel, single-row processing of data frames.
##' 
##' @param X The data frame, each row of which will be processed using
##' \code{FUN}
##' @param FUN A function whose first argument is a single-row data frame, i.e.
##' a single row of \code{X}.  The value returned by \code{FUN} can be any
##' object
##' @param \dots Additional named arguments to \code{FUN}
##' @param output.df logical indicating whether the value returned by
##' \code{dfplapply} should be a data frame. If \code{output.df = TRUE}, then
##' the value returned by \code{FUN} should be a data frame.  If
##' \code{output.df = FALSE}, a list is returned by \code{dfplapply}.
##' @param packages Character vector giving the names of packages that will be
##' loaded in each new instance of R.
##' @param header.file Optional text string indicating a file that will be
##' initially sourced in each instance of R.  Typically this would be used to
##' create an 'environment' that will satisfy all potential dependencies for
##' \code{FUN}. If \code{NULL}, no file is sourced.
##' @param needed.objects Character vector giving the names of objects which
##' reside in the evironment specified by \code{needed.objects.env} that may be
##' needed by \code{FUN} which are loaded into the GLOBAL ENVIRONMENT of each
##' new instance of R that is launched.  If \code{NULL}, no additional objects
##' are passed.
##' @param needed.objects.env Environment where \code{needed.objects} reside.
##' This defaults to the environment in which \code{dfplapply} is called.
##' @param jobName Text string indicating the prefix for files that will be
##' created while launching the separate instances of R.
##' @param njobs The number of jobs (subsets), which should probably not exceed
##' the number of cores on the machine.
##' @param max.hours The maximum number of hours to wait for the \code{njobs}
##' to complete.
##' @param check.interval.sec The number of seconds to wait between checking to
##' see whether all \code{njobs} have completed.
##' @param collate \code{=TRUE} creates a 'first-in-first-out' processing of
##' the elements of the list \code{X}.  This logical is passed to the
##' \code{collate} argument of \code{\link{parseJob}}.
##' @param random.seed An integer setting the random seed, which will result in
##' randomizing the rows of the input data frame, \code{X}, that are assigned
##' to each job. This is useful when the computing time for each row varies
##' significantly.  If \code{NULL}, no randomization is performed and the rows
##' of the input data frame \code{X} are subdivided sequentially among the
##' jobs.  This variable is passed to the \code{random.seed} argument of
##' \code{\link{parseJob}}.
##' @param clean.up \code{=TRUE} will delete temporary files.
##' @param rout \code{=TRUE} will gather the \code{njobs} *.Rout files into a
##' single file named "jobName_YYYY-MM-DD_HHMMSS.Rout" which will not be
##' deleted.
##' @param verbose \code{=TRUE} prints messages which show the progress of the
##' jobs.
##' @return A list or data frame containing the results of processing each row
##' of \code{X} with \code{FUN}.
##' @author Landon Sego
##' @seealso \code{\link{plapply}}, \code{\link{lapply}}
##' @keywords misc
##' @examples
##' 
##' 
##' X <- data.frame(a=1:5, b=letters[1:5])
##' 
##' # Function with a single data frame as output
##' test.1 <- function(x)
##'   list(ab = paste(x$a,x$b,sep="-"), a2 = x$a^2, bnew = paste(x$b, "new", sep="."))
##' 
##' # Data frame output
##' dfplapply(X, test.1, output.df=TRUE, njobs=3, check.interval.sec=1)
##' # List output
##' dfplapply(X, test.1, njobs=3, check.interval.sec=1)
##' 
##' # Function with 2 rows of output
##' test.2 <- function(x)
##'   data.frame(ab = rep(paste(x$a,x$b,sep="-"), 2), a2 = rep(x$a^2, 2))
##' 
##' dfplapply(X, test.2, output.df=TRUE, njobs=3, check.interval.sec=1, verbose=TRUE)
##' 
##' # Passing in objects
##' a.out <- 10
##' test.3 <- function(x)
##'   data.frame(a = x$a + a.out, b = paste(x$b, a.out, sep="-"))
##' 
##' dfplapply(X, test.3, output.df=TRUE, needed.objects="a.out", njobs=2, check.interval.sec=1)
##' 
##' 
##' 
dfplapply <- function(X, FUN, ...,
                      output.df = FALSE,
                      packages = NULL,
                      header.file = NULL,
                      needed.objects = NULL,
                      needed.objects.env = parent.frame(),
                      jobName = "dfplapply",
                      njobs = 7,
                      max.hours = 24,
                      check.interval.sec = 30,
                      collate = FALSE,
                      random.seed = NULL,
                      clean.up = TRUE,
                      rout = !clean.up,
                      verbose = FALSE) {


  if (!is.data.frame(X))
    stop("'", deparse(substitute(X)), "' is not a data frame\n")    

  # Process it with plapply
  X.out <- plapply(df2list(X, out.type = "data.frame"), FUN, ...,
                   packages = packages,
                   header.file = header.file,
                   needed.objects = needed.objects,
                   needed.objects.env = needed.objects.env,
                   jobName = jobName,
                   njobs = njobs,
                   max.hours = max.hours,
                   check.interval.sec = check.interval.sec,
                   collate = collate,
                   random.seed = random.seed,
                   clean.up = clean.up,
                   rout = rout,
                   verbose = verbose)

  # Collapse results to a data frame if requested
  if (output.df) 
    X.out <- list2df(X.out)

  return(X.out)
  
} # dfplapply



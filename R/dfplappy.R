##' Parallelized single row processing of a data frame
##'
##' Applies a function to each row of a data frame in a parallelized fashion
##' (by submitting multiple batch R jobs)
##'
##' \code{dfplapply} is a convenient wrapper for \code{plapply}, modified
##' especially for parallel, single-row processing of data frames.
##'
##' @export
##'
##' @param X The data frame, each row of which will be processed using
##' \code{FUN}
##' 
##' @param FUN A function whose first argument is a single-row data frame, i.e.
##' a single row of \code{X}.  The value returned by \code{FUN} can be any
##' object
##' 
##' @param output.df logical indicating whether the value returned by
##' \code{dfplapply} should be a data frame. If \code{output.df = TRUE}, then
##' the value returned by \code{FUN} should be a data frame.  If
##' \code{output.df = FALSE}, a list is returned by \code{dfplapply}.
##' 
##' @inheritParams plapply
##'
##' @return A list or data frame containing the results of processing each row
##' of \code{X} with \code{FUN}.
##' 
##' @author Landon Sego
##' 
##' @seealso \code{\link{plapply}}
##' 
##' @keywords misc
##' 
##' @examples
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
dfplapply <- function(X, FUN, ...,
                      output.df = FALSE,
                      packages = NULL,
                      header.file = NULL,
                      needed.objects = NULL,
                      needed.objects.env = parent.frame(),
                      jobName = "dfplapply",
                      njobs = parallel::detectCores() - 1,
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



##' Parallelized single row processing of a data frame
##'
##' Applies a function to each row of a data frame in a parallelized fashion
##' (by submitting multiple batch R jobs).  It is a convenient wrapper for \code{\link{plapply}}, modified
##' especially for parallel, single-row processing of data frames.
##'
##' @export
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
##' X <- data.frame(a = 1:3, b = letters[1:3])
##'
##' \donttest{
##' # Function that will operate on each of x, producing a simple list
##' test.1 <- function(x) {
##'   list(ab = paste(x$a, x$b, sep = "-"), a2 = x$a^2, bnew = paste(x$b, "new", sep = "."))
##' }
##'
##' # Data frame output
##' dfplapply(X, test.1, output.df = TRUE, njobs = 2)
##'
##' # List output
##' dfplapply(X, test.1, njobs = 2)
##'
##' # Function with 2 rows of output
##' test.2 <- function(x) {
##'   data.frame(ab = rep(paste(x$a, x$b, sep = "-"), 2), a2 = rep(x$a^2, 2))
##' }
##'
##' dfplapply(X, test.2, output.df = TRUE, njobs = 2, verbose = TRUE)
##' }
##'
##' # Passing in other objects needed by FUN
##' a.out <- 10
##' test.3 <- function(x) {
##'   data.frame(a = x$a + a.out, b = paste(x$b, a.out, sep="-"))
##' }
##'
##' dfplapply(X, test.3, output.df = TRUE, needed.objects = "a.out", njobs = 2)

dfplapply <- function(X, FUN, ...,
                      output.df = FALSE,
                      njobs = parallel::detectCores() - 1,
                      packages = NULL,
                      header.file = NULL,
                      needed.objects = NULL,
                      needed.objects.env = parent.frame(),
                      workDir = "plapply",
                      clobber = TRUE,
                      max.hours = 24,
                      check.interval.sec = 1,
                      collate = FALSE,
                      random.seed = NULL,
                      rout = NULL,
                      clean.up = TRUE,
                      verbose = FALSE) {

  # Check args not checked by plapply()
  stopifnotMsg(is.data.frame(X),
               paste("'", deparse(substitute(X)), "' is not a data frame", sep = ""),
               is.logical(output.df) & length(output.df) == 1,
               "'output.df' must be TRUE or FALSE")

  # Process it with plapply
  X.out <- plapply(df2list(X, out.type = "data.frame"), FUN, ...,
                   njobs = njobs,
                   packages = packages,
                   header.file = header.file,
                   needed.objects = needed.objects,
                   needed.objects.env = needed.objects.env,
                   workDir = workDir,
                   clobber = clobber,
                   max.hours = max.hours,
                   check.interval.sec = check.interval.sec,
                   collate = collate,
                   random.seed = random.seed,
                   rout = rout,
                   clean.up = clean.up,
                   verbose = verbose)

  # Collapse results to a data frame if requested
  if (output.df) {
    X.out <- list2df(X.out)
  }

  return(X.out)

} # dfplapply

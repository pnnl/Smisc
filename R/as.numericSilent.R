##' Convert any vector to numeric, if possible
##'
##' @export
##' @param x vector of any type
##'
##' @return If \code{as.numeric(x)} produces an error or warning, \code{x} is
##' returned unchanged.  Otherwise, \code{as.numeric(x)} is returned.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{as.numeric}}
##'
##' @keywords misc
##'
##' @examples
##' as.numericSilent(c("this","that"))
##' as.numericSilent(c("2893.9","9423.48"))
##' as.numericSilent(c("392.1", "that"))

as.numericSilent <- function(x) {

  # Set warnings to errors
  op <- options(warn = 2)

  # If an error results, the data aren't numeric
  if (class(x.num <- try(as.numeric(x), silent = TRUE)) == "try-error") {
    x.num <- x
  }

  # Restore the previous setting
  options(op)

  return(x.num)

} # as.numericSilent()

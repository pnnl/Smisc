##' A simple function for converting factors to numeric values
##'
##' @export
##' @param x A vector of type \code{factor}
##'
##' @return \code{x} converted to a numeric vector
##'
##' @examples
##' # Define a factor object
##' y <- factor(5:7)
##' y
##'
##' # incorrectly convert to numeric
##' as.numeric(y)
##'
##' # correctly convert
##' factor2numeric(y)
##'

factor2numeric <- function(x) {

  stopifnot(is.factor(x))

  return(as.numeric(levels(x))[x])

} # factor2numeric

##' Non-vectorized version of ifelse
##'
##' Non-vectorized version of \code{\link{ifelse}}.
##'
##' @export
##' 
##' @param test An expression that resolves to logical of length 1, i.e.,
##' \code{TRUE} or \code{FALSE}
##' 
##' @param yes An object that is returned if \code{test} resolves to \code{TRUE}
##' 
##' @param no An object that is returned if \code{test} resolves to \code{FALSE}
##' 
##' @return Either \code{yes} or \code{no}, depending on \code{test}.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{ifelse}}
##'
##' @examples
##' ifelse1(7 == 7, rnorm(5), letters[1:5])
##' ifelse1(7 == 2, rnorm(5), letters[1:5])

ifelse1 <- function(test, yes, no) {

  stopifnot(length(test) == 1,
            is.logical(test))

  if (test)
    return(yes)
  else
    return(no)
    
} # ifelse1

##' Generic for \code{signal}
##'
# The missingpreds method is here as an example of adding another generic to this file.  It doesn't have any relevance to Smisc
#
## @aliases missingpreds 
##' 
##' @usage signal(object, ...)
## missingpreds(object, ...)
##' 
##' @param object The object on which the generic operates
##' 
##' @param \dots Arguments passed to specific methods
##'
##' @rdname generics
##' 
##' @export
signal <- function (object, ...) {
  UseMethod("signal", object)
}

## @export
## missingpreds <- function (object, ...) {
##   UseMethod("missingpreds", object)
## }

##' Generic for \code{signal}
##'
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

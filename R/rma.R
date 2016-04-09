##' Remove all objects from the global environment
##'
##' An alias for \code{rm(list = ls())}.
##'
##' @export
##' @author Landon Sego
##' @keywords misc
rma <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
} # rma

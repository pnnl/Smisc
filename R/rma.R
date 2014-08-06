##' Remove all objects from the global environment
##' 
##' Remove all objects from the global environment
##' 
##' Alias for \code{rm(list=ls())}.
##' 
##' @author Landon Sego
##' @keywords misc
rma <- function() {
  rm(list=ls(envir=.GlobalEnv), envir=.GlobalEnv)
} # rma

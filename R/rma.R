# Alias for removing all objects in the Global Environment


##' Remove all objects from the global environment
##' 
##' Remove all objects from the global environment
##' 
##' Alias for \code{rm(list=ls())}.
##' 
##' @usage rma()
##' @author Landon Sego
##' @keywords misc
rma <- function() {
  rm(list=ls(envir=.GlobalEnv), envir=.GlobalEnv)
} # rma

# Alias for removing all objects in the Global Environment
rma <- function() {
  rm(list=ls(envir=.GlobalEnv), envir=.GlobalEnv)
} # rma

# This function would be called when namespace is unloaded (if it were not in the
# search path because it had been loaded via loadNamespace("Smisc")
.onUnload <- function(libpath) {

  if (is.loaded("smartFilter", PACKAGE = "Smisc")) {

    confirm <- try(library.dynam.unload("Smisc", libpath), silent = TRUE)

    if (class(confirm) != "try-error")
      cat("Smisc shared objects are unloaded\n")
    else
      cat(confirm,"\n")
  }
  
}


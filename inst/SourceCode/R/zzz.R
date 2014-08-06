## This is for pre R-2.14.0
## # This function is called when the library is loaded.  It loads the dll's and gives an introductory message.
## .First.lib <- function(libname, pkgname) {
  
##   library.dynam("pnlStat", package=pkgname)

##   cat("\nWelcome to the pnlStat package, version ", packageDescription("pnlStat", fields="Version"), ".\n\n",
##       "Commented source code can be found in\n", path.package(package="pnlStat"), "/SourceCode.\n\n", sep="")
  
## }

# For R-2.14.0 and beyond
.onAttach <- function(libname, pkgname) {

  banner.text <- paste("\nWelcome to the pnlStat package, version ", packageDescription("pnlStat", fields="Version"), ".\n\n",
                       "Commented source code can be found in\n", path.package(package="pnlStat"), "/SourceCode.\n", sep="")

  packageStartupMessage(banner.text)
  
}

# This function is called when the library is detached. It unloads the dll's.
.Last.lib <- function(libpath) {
  confirm <- try(library.dynam.unload("pnlStat", libpath), silent = TRUE)
  if (class(confirm) != "try-error")
    cat("pnlStat shared objects are unloaded\n")
  else
    cat(confirm,"\n")
}

# This function would be called when namespace is unloaded (if it were not in the search path
# because it had been loaded via loadNamespace("pnlStat")
.onUnload <- function(libpath) {

  if (is.loaded("smartFilter", PACKAGE = "pnlStat")) {
  
    confirm <- try(library.dynam.unload("pnlStat", libpath), silent = TRUE)
    
    if (class(confirm) != "try-error")
      cat("pnlStat shared objects are unloaded\n")
    else
      cat(confirm,"\n")
  }
}


.onAttach <- function(libname, pkgname) {

  banner.text <- paste("\nWelcome to the Smisc package, version ", packageDescription("Smisc", fields="Version"), ".\n\n",
                       "Commented source code can be found in\n", path.package(package="Smisc"), "/SourceCode.\n", sep="")

  packageStartupMessage(banner.text)

}

# This function is called when the library is detached. It unloads the dll's.
.Last.lib <- function(libpath) {
  confirm <- try(library.dynam.unload("Smisc", libpath), silent = TRUE)
  if (class(confirm) != "try-error")
    cat("Smisc shared objects are unloaded\n")
  else
    cat(confirm,"\n")
}

# This function would be called when namespace is unloaded (if it were not in the search path
# because it had been loaded via loadNamespace("Smisc")
.onUnload <- function(libpath) {

  if (is.loaded("smartFilter", PACKAGE = "Smisc")) {

    confirm <- try(library.dynam.unload("Smisc", libpath), silent = TRUE)

    if (class(confirm) != "try-error")
      cat("Smisc shared objects are unloaded\n")
    else
      cat(confirm,"\n")
  }
}


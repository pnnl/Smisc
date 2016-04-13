##' Separate a list into distinct objects
##'
##' Separate a list into distinct objects
##'
##' The names of the objects are determined by the names in the list.  If names in the list are not present, the objects
##' are named o1, o2, o3, etc.
##'
##' @author Landon Sego
##'
##' @export
##' @param X a list to be separated
##'
##' @param envir The environment where the objects in the list are assigned, defaults to \code{parent.frame()}, the environment
##' where \code{sepList} was called.
##'
##' @param objNames Character vector indicating the names of the objects that will be created.  The length of
##' \code{names} must match the length of the list, \code{X}.
##'
##' @param verbose Logical indicating whether to print the names of the objects that have been created by splitting
##' the list
##'
##' @return Invisibly returns a character vector of the names of the objects that were created, and assigns the objects to
##' the environment specified by \code{envir}
##'
##' @examples
##'# Simplest way to use sepList()
##'aList <- list(a = 1:10,  b = letters[1:5], d = TRUE)
##'sepList(aList)
##'ls()
##'a
##'b
##'d
##'
##'# Keeping the object names, and listing them via "verbose"
##'objs <- sepList(list(1:5, c("bits", "bytes"), c(TRUE, FALSE)), verbose = TRUE)
##'objs
##'o1
##'o2
##'o3
##'
##'# Note that it doesn't recurse into sublists, only the top level object
##'# a and b are created
##'sepList(list(a = 1:2, b = list(b1 = 5, b2 = FALSE)), verbose = TRUE)
##'
##'# Separate the original list inside a function, notice where the objects are written
##'sepTest <- function(x) {
##'
##'  # Keep objects inside the local environment
##'  cat("Objects in the local environment before separating the list:\n")
##'  print(ls())
##'
##'  sepList(x)
##'
##'  cat("Objects in the local environment after separating the list:\n")
##'  print(ls())
##'
##'  # Place objects in the global environment instead
##'  cat("Objects in the global environment before separating the list:\n")
##'  print(ls(.GlobalEnv))
##'
##'  sepList(x, envir = .GlobalEnv)
##'
##'  cat("Objects in the local environment after separating the list:\n")
##'  print(ls(.GlobalEnv))
##'
##'} # sepTest
##'
##'sepTest(list(z1 = 10, z2 = "that"))
##'
##'# Clean up example objects
##'rm(aList, a, b, d, objs, o1, o2, o3, sepTest, z1, z2)

sepList <- function(X, envir = parent.frame(), objNames = names(X), verbose = FALSE) {

  # Check arguments
  stopifnot(is.list(X),
            is.environment(envir),
            is.logical(verbose))

  # If the list does not have names, create some
  if (is.null(objNames)) {
    objNames <- paste("o", 1:length(X), sep = "")
  }

  # Check length of names
  stopifnot(length(objNames) == length(X))

  # Now assign the elements of the list to the desired environment
  for (i in 1:length(X)) {
    assign(objNames[i], X[[i]], envir = envir)
  }

  # Print the names if verbose
  if (verbose) {

    # It would be nice to actually print the name of the environment here, but I couldn't find a way to do it
    cat("The following objects were written to the specified environment:\n",
        paste(objNames, collapse = ", "), "\n")
  }

  # Invisibly return the object names
  invisible(objNames)

} # sepList

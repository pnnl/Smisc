##' Sources all files with '.R' or '.r' extensions in a directory
##'
##' Sources all files with '.R' or '.r' extensions in a directory using a try-catch for each file
##'
##' In addition to sourcing files for general use, this function is also useful in package development to
##' verify there are no syntax errors prior to building and compilation.
##'
##' @export
##' @param directory Character string indicating the path of the directory
##' containing the R files to be sourced.
##'
##' @param recursive \code{=TRUE} descends into subdirectories of
##' \code{directory}
##'
##' @param tryCatch if \code{TRUE}, sourcing is protected in a try catch, i.e.,
##' if there is an error, \code{sourceDir} will continue to the next file.  If
##' \code{FALSE}, \code{sourceDir} will stop if a file sources with an error.
##'
##' @param \dots Additional arguments to \code{\link{source}}
##'
##' @return Invisibly returns a character vector containing the files that were
##' identified for sourcing. Also prints a message indicating whether each file was sourced correctly or
##' not.
##' 
##' @author Landon Sego
##' 
##' @keywords misc
##'
sourceDir <- function(directory, recursive = FALSE, tryCatch = TRUE, ...) {

  x <- dir(directory, full.names = TRUE, recursive = recursive)
 
  files <- x[toupper(substr(x, nchar(x) - 1, nchar(x))) == ".R"]
 
  if (length(files)) {
 
    for (i in files) {
 
      if (tryCatch) {
 
        if (class(err <- try(source(i, ...), silent = TRUE)) != "try-error")
          cat("Sourcing '", i, "'\n", sep = "")
        else
          cat("File '", i, "' failed to source correctly:\n",
              err, sep = "")
      }
      else {
        cat("Sourcing '", i, "'\n", sep = "")
        source(i, ...)
      }
 
    } # for (i in files)
 
  } # if length(files)
 
  else {
    cat("There are no files with '.R' or '.r' extensions in", directory, "\n")
  }
 
  invisible(files)

} # end sourceDir()

# Source all the .R (or .r) files in a directory.



##' 'sources' all files with '.R' or '.r' extensions in a directory
##' 
##' 
##' 
##' @usage sourceDir(directory, recursive = FALSE, tryCatch = TRUE, ...)
##' @param directory Character string indicating the path of the directory
##' containing the R source files
##' @param recursive \code{=TRUE} descends into subdirectories of
##' \code{directory}
##' @param tryCatch if \code{TRUE}, sourcing is protected in a try catch, i.e.,
##' if there is an error, \code{sourceDir} will continue to the next file.  If
##' \code{FALSE}, \code{sourceDir} will stop if a file sources with an error.
##' @param \dots Additional arguments to \code{source} function
##' @return Invisibly returns a character vector containing the files that were
##' identified for sourcing.
##' 
##' Also prints a message indicating whether each file was sourced correctly or
##' not.
##' @author Landon Sego
##' @keywords misc
##' @examples
##' 
##' # Source R files in the current directory
##' sourceDir(".")
##' 
##' # Source R files in the directory "c:\Work\myRcode"
##' # sourceDir("c:/Work/myRcode")
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

 else
   cat("There are no files with '.R' or '.r' extensions in", directory, "\n")

 invisible(files)
 
} # end sourceDir()

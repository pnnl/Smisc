##' Display the contents of a file to the R terminal
##' 
##' Display the contents of a file to the R terminal, like the \code{more} command 
##'
##' @export
##'
##' @param file Text string giving the file name
## @param method Text string indicating one of \code{"all"}, \code{"head"}, or \code{"tail"}.  See Details.
##' @param n Integer specifying the maximum number of lines to display.  This is passed to the \code{n} argument
##' in \code{\link{readLines}}.
##'
##' @return Invisibly returns the contents of the file are displayed on the R console
##'
##' @examples
##' cat("Here's a file\n", "with a few lines\n", "to read.\n", sep = "", file = "tmpFile.txt")
##' splashFile("tmpFile.txt")
##' unlink("tmpFile.txt")
##' 

splashFile <- function(file, n = 500) {

  # Check inputs
  stopifnot(is.character(file),
            file.exists(file),
            is.numeric(n))

  # Read the file
  fileIn <- readLines(file, n = n)

  # Now splash it to the screen
  cat(paste(fileIn, collapse = "\n"), "\n")

  invisible(NULL)
    
} # more

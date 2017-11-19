##' Display the contents of a text file to the R console
##'
##' @export
##' @param file Text string giving the file name
##'
##' @param n Integer specifying the maximum number of lines to read from the file. This is passed
##' to the \code{n} argument of \code{\link{readLines}}. The default is \code{-1},
##' which will read all the lines in the file.
##'
##' @param display Text string that uniquely identifies one of \code{"all"}, \code{"head"},
##' or \code{"tail"}.  Defaults to \code{"all"}, which causes all lines read from the file
##' to be displayed, \code{"head"} shows the first 6 lines that were read, and \code{"tail"} shows
##' the last 6 lines that were read.
##'
##' @return Returns nothing, but it does display the contents of the file on the R console.
##'
##' @seealso \code{\link{readLines}}
##'
##' @author Landon Sego
##'
##' @examples
##' cat("Here's a file\n", "with a few lines\n",
##'     "to read.\n", sep = "", file = "tmpFile.txt")
##' more("tmpFile.txt")
##' unlink("tmpFile.txt")

more <- function(file, n = -1, display = c("all", "head", "tail")) {

  # Check inputs
  stopifnotMsg(# file
               if (is.character(file) & (length(file) == 1)) {
                 file.exists(file)
               } else FALSE,
               "'file' must be a character string and the file must exist",

               # n
               if (is.numeric(n) & (length(n) == 1)) {
                 (n >= -1) & (n %% 1 == 0)
               } else FALSE,
               "'n' must be in one of -1, 0, 1, 2, ...")

  if (!is.integer(n)) {
    n <- as.integer(n)
  }

  display <- match.arg(display)
  
  # Read the file
  fileIn <- readLines(file, n = n)

  # Get head or tail if requested
  if (display == "head") {
    fileIn <- head(fileIn)
  }
  else if (display == "tail") {
    fileIn <- tail(fileIn)
  }

  # Now send file contents to the screen
  cat(paste(fileIn, collapse = "\n"), "\n")

} # more

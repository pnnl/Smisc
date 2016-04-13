##' Remove the extension of a vector of filenames
##' 
##' Remove the extension of a vector of filenames, assuming that the extension is the set of characters
##' that follows the last \code{"."}
##'
##' Assumes paths are delineated using forward slashes.  If an \code{NA} is
##' supplied, then an \code{NA} is returned.  If the desired string doesn't
##' exist (see examples below), a \code{""} is returned.
##'
##' @export 
##'
##' @param vec Character vector (usually containing filenames)
##'
##' @param split.char A single character used to split the character strings
##'
##' @return Character vector with the last "." and the
##' filename extension removed.  Alternatively, another split character could
##' be used.
##' 
##' @author Landon Sego
##'
##' @seealso Additional functions for filename manipulations:  \code{\link{getExtension}}, \code{\link{stripPath}},
##' \code{\link{getPath}}, \code{\link{grabLast}}, \code{\link{basename}}, \code{\link{dirname}}
##'
##' @keywords misc
##'
##' @examples
##' stripExtension(c("this old file.doc", "that young file.rtf",
##'                  "this.good.file.doc", "this_bad_file"))
##'
##' stripExtension(c("this old file*doc", "that young file*rtf",
##'                  "this*good*file*doc", "this_bad_file"), split.char = "*")
##'
##'
##' # Named vectors are not required, but are included here to make the
##' # output easier to read.  This example demonstrates a number of
##' # pathological cases.
##' stripExtension(c(a = NA, b = ".doc", c = "this.pdf", d = "this.file.", e = ".",
##'                  f = "noExtension", g = "direc.name/filename.txt", h = ""))
##' 
##' # An example with 'real' files
##' files <- dir(file.path(path.package(package = "Smisc"), "data"), full.names = TRUE)
##' print(files)
##' stripExtension(files)
##' stripExtension(stripPath(files))

stripExtension <- function(vec, split.char = ".") {

  # Check arguments
  stopifnotMsg(is.character(vec),
               "'vec' must be a character vector",
               if (is.character(split.char) & (length(split.char) == 1)) {
                 nchar(split.char) == 1
               } else FALSE,
               "'split.char' must be a single character")
  
  # If the last character is the split character then flag it so that nothing will be returned
  check.for.ending.period <- function(x) {

     if (!is.na(x)) {
       n <- nchar(x)
       if (substr(x, n, n) == split.char)
         x <- paste(x, "BAD-EXTENSION-NO-ONE-WOULD-NAME-THEIR-EXTENSION-THIS", sep = "")
     }

     return(x)

  } # check.for.ending.period

  names.vec <- names(vec)
  vec <- tapply(vec, 1:length(vec), check.for.ending.period)

  # Function to rejoin any filenames that may have more than one
  # period
  rejoin <- function(v1) {

    if ((len <- length(v1)) > 1)
      return(paste(v1[1:(len-1)], collapse = split.char))

    else
      return(v1)

  } # end rejoin

  # If paths exist in the filename, we don't want to cut at "." that are in directories...
  # Strip the extension in the last element of the sub-list when we split by "/"
  sE <- function(v2) {

    if (length(v2) == 1) {
      if (is.na(v2))
        return(NA)
    }

    v2[length(v2)] <- unlist(lapply(strsplit(v2[length(v2)], split.char, fixed = TRUE), rejoin))
    paste(v2, collapse = "/")
  }

  result <- unlist(lapply(strsplit(vec, "/", fixed = TRUE), sE))

  names(result) <- names.vec

  return(result)

} # end stripExtension()


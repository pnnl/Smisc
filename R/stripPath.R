##' Remove the path from a vector of filenames
##' 
##' Remove the path from a vector of filenames
##'
##' Assumes paths are delineated using forward slashes.  If an \code{NA} is
##' supplied, then an \code{NA} is returned.  If the desired string doesn't
##' exist (see examples below), a \code{""} is returned.
##'
##' @export 
##' @inheritParams stripExtension
##'
##' @return Character vector with leading path removed from the filenames
##' 
##' @author Landon Sego
##'
##' @seealso Additional functions for filename manipulations:  \code{\link{stripExtension}}, \code{\link{getExtension}},
##' \code{\link{getPath}}, \code{\link{grabLast}}, \code{\link{basename}}, \code{\link{dirname}}
##'
##' @keywords misc
##'
##' @examples
##' stripPath(c(a = "this.good.path/filename.R", b = "nopath.R", c = "/", d = NA,
##'             e = "only.paths.1/only.paths.2/", ""))
##'
##' # An example with 'real' files
##' files <- dir(file.path(path.package(package = "Smisc"), "data"), full.names = TRUE)
##' print(files)
##' stripPath(files)
##' stripPath(stripExtension(files))

stripPath <- function(vec) {

  # If only a filename with no path comes in, we should still return the filename, so prepend
  # the 'vec' with '/'
  vec1 <- paste("/", vec, sep  ="")
  vec1[is.na(vec)] <- NA
  names(vec1) <- names(vec)

  # Grab the text string that follows the last '/'
  return(grabLast(vec1, "/"))

} # stripPath


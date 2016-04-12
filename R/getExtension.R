##' Get the extension of a vector of filenames
##' 
##' Get the extension of a vector of filenames, assuming that the extension is the set of characters that
##' follows the last \code{"."}.  A wrapper for \code{\link{grabLast}}.
##'
##' Assumes paths are delineated using forward slashes.  If an \code{NA} is
##' supplied, then an \code{NA} is returned.  If the desired string doesn't
##' exist (see examples below), a \code{""} is returned.
##'
##' @export 
##' @inheritParams stripExtension
##'
##' @return Character vector of filename extensions
##' 
##' @author Landon Sego
##'
##' @seealso Additional functions for filename manipulations:  \code{\link{stripExtension}}, \code{\link{stripPath}},
##' \code{\link{getPath}}, \code{\link{grabLast}}, \code{\link{basename}}, \code{\link{dirname}}
##'
##' @keywords misc
##'
##' @examples
##' getExtension(c(a = "this old file.doc",
##'                b = "that young file.rtf",
##'                c = "this.good.file.doc",
##'                d = "this_bad_file",
##'                e = "thisfile.",
##'                f = NA,
##'                g = "that.this.pdf",
##'                h = ".", i = ""))
##'
##' # An example with 'real' files
##' files <- dir(file.path(path.package(package = "Smisc"), "data"), full.names = TRUE)
##' print(files)
##' getExtension(files)

getExtension <- function(vec, split.char = ".") {

  grabLast(vec, split.char)

} # getExtension


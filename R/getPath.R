##' Get the path of a vector of filenames
##' 
##' Get the path of a vector of filenames
##'
##' Assumes paths are delineated using forward slashes.  If an \code{NA} is
##' supplied, then an \code{NA} is returned.  If the desired string doesn't
##' exist (see examples below), a \code{""} is returned.
##'
##' @export 
##' @inheritParams stripExtension
##'
##' @return Character vector with pathnames only, the filename removed
##' 
##' @author Landon Sego
##'
##' @seealso Additional functions for filename manipulations:  \code{\link{stripExtension}}, \code{\link{getExtension}},
##' \code{\link{stripPath}}, \code{\link{grabLast}}, \code{\link{basename}}, \code{\link{dirname}}
##'
##' @keywords misc
##'
##' @examples
##' getPath(c(a="this.good.path/filename.R", b="nopath.R", c="/", d=NA,
##'           e="path1/path2/", ""))
##' 
##' # An example with 'real' files
##' files <- dir(file.path(path.package(package = "Smisc"), "data"), full.names = TRUE)
##' print(files)
##' getPath(files)

getPath <- function(vec) {

  # Verify that 'vec' is a character vector
  if (!is.character(vec))
    stop("'", deparse(substitute(vec)), "' must be a character vector.\n")

  names.vec <- names(vec)

  # If no slashes, it should return "."
  # If ends with a slash, it should return the whole string minus the slash at the end
  preprocess <- function(x) {

    if (is.na(x))
      return(NA)
    else if (x == "")
      return("/")
    else if (!length(grep("/", x)))
      return(paste("./", x, sep=""))
    else if (substr(x, nchar(x), nchar(x)) == "/")
      return(paste(x, "tmpJunk", sep=""))
    else
      return(x)

  } # preprocess

  vec <- tapply(vec, 1:length(vec), preprocess)

  # Function to rejoin any filenames that may have more than one
  # period
  rejoin <- function(v1) {

    if (length(v1) == 1) {
      if (is.na(v1))
        return(NA)
    }

    if ((len <- length(v1)) > 1)
      return(paste(v1[1:(len-1)], collapse="/"))

    else
      return(v1)

  } # end rejoin

  # Return the results
  out <- unlist(lapply(strsplit(vec, "/"), rejoin))
  names(out) <- names.vec

  out

} # getPath



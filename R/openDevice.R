##' Opens a graphics device based on the filename extension
##'
##' Opens the appropriate graphics device based on the filename extension
##'
##' Will open a \code{ps}, \code{pdf}, \code{jpg}, \code{tif}, \code{png}, or
##' \code{bmp} device using one of the respective graphics functions:
##' \code{\link{postscript}}, \code{\link{pdf}}, \code{\link{jpeg}},
##' \code{\link{tiff}}, \code{\link{png}}, or \code{\link{bmp}}.
##'
##' \code{openDevice} silently ignores named arguments in \code{\dots{}} which
##' do not match the named arguments in the graphics device function that is
##' called.
##'
##' @export
##'
##' @param fileName Character string giving the filename for the graphics
##' output.  It must contain one of the extensions listed in the \code{Details}
##' below.
##'
##' @param \dots Named arguments to the device functions listed in
##' \code{Details} below.
##' 
##' @return The graphics device is opened and the filename is invisibly
##' returned.
##' 
##' @author Landon Sego
##' 
##' @keywords misc
##' 
##' @examples
##'
##' # Open 3 example devices
##' openDevice("ex1.pdf", height=6, width=12)
##' plot(1:10, 1:10)
##' 
##' openDevice("ex1.jpg")
##' plot(1:10, 1:10)
##'
##' openDevice("ex1.png")
##' plot(1:10, 1:10)
##'
##' # List the devices and their filenames
##' dev.list()
##' dir(pattern = "ex1")
##'
##' # Turn each of the 3 devices off
##' for (i in 1:3)
##'   dev.off(dev.list()[length(dev.list())])
##'
##' # Delete the created files
##' unlink(c("ex1.pdf","ex1.png","ex1.jpg"))
##'
##' # List the current devices
##' dev.list()
##'
openDevice <- function(fileName, ...) {

  fileName <- path.expand(fileName)

  ext <- tolower(getExtension(fileName))

  supported.extensions <- c("ps","pdf","jpg","tif","png","bmp")

  if (!(ext %in% supported.extensions)) {
    stop("Filename must have one of the following extensions: '",
         paste(supported.extensions, collapse="', '"), "'.\n")
  }

  # Assign the function to be used
  fname <- switch(ext,
                  ps = "postscript",
                  pdf = "pdf",
                  jpg = "jpeg",
                  tif = "tiff",
                  png = "png",
                  bmp = "bmp")
  
  # Get the argument names of the function that will be called
  argNames <- names(formals(fname))

  # Add the appropriate first argument name to our list
  vList <- list(fileName, ...)
  names(vList)[1] <- argNames[1]

  # In the call, drop any arguments from our list that are not in the arguments
  # of the desired function and open the device
  do.call(fname, vList[intersect(names(vList), argNames)])

  # invisibly return the filename
  invisible(fileName)

} # openDevice


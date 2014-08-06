##' Imbeds the present datetime into a file name
##'
##'
##' @export
##'
##' @param description Character string giving the base name of the file
##' @param extension Character string giving the extension of the file
##' (excluding the period)
##' @return Character string of the form
##' \code{description_YYYY-MM-DD_HHMMSS.extension}
##' @author Landon Sego
##' @keywords misc
##' @examples
##'
##' timeStamp("aFilename", "txt")
##'
timeStamp <- function(description,extension) {

  # Verify that inputs are character
  if (!(is.character(description) & is.character(extension)))
    stop("Arguments to timeStamp() must be character.\n")

  # Returns a text string of the form:
  # description_YYYY-MM-DD_HHMMSS.extension

  dtime <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

  return(paste(description, "_", dtime, ".", extension,sep=""))

}  # timeStamp

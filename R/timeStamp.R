##' Embeds the present datetime into a file name
##'
##' @export
##' @param description Character vector giving the base name(s) of the file(s)
##'
##' @param extension Character vector giving the extension(s) of the file(s)
##' (excluding the period)
##'
##' @return Character string of the form \code{description_YYYY-MM-DD_HHMMSS.extension}
##'
##' @author Landon Sego
##'
##' @keywords misc
##'
##' @examples
##'
##' timeStamp("aFilename", "txt")

timeStamp <- function(description, extension) {

  # Verify that inputs are character
  stopifnotMsg(is.character(description) &
               is.character(extension),
               "Arguments must be character")

  # Returns a text string of the form:
  # description_YYYY-MM-DD_HHMMSS.extension

  dtime <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

  return(paste(description, "_", dtime, ".", extension, sep = ""))

}  # timeStamp

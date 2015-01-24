##' Quickly row and column bind many objects together at once
##'
##' A wrapper for \code{\link{rbind}} and \code{\link{cbind}} to quickly bind
##' numerous objects together at once using a single call to \code{\link{rbind}} or
##'  \code{\link{cbind}}. This function is most helpful when there are
##' many objects to bind.
##'
##' @export
##' 
##' @param objects A character vector with the names of the objects to be bound together
##'
##' @param type A single character, "r" for \code{\link{rbind}} or "c" for
##' \code{\link{cbind}}.  Default to "r".
##'
##' @return The binded object
##'
##' @author Landon Sego
##'
##' @examples
##' # Row binding
##' a1 <- data.frame(a = 1:3, b = rnorm(3), c = runif(3))
##' a2 <- data.frame(a = 4:6, b = rnorm(3), c = runif(3))
##' a3 <- data.frame(a = 7:9, b = rnorm(3), c = runif(3))
##'
##' qbind(paste("a", 1:3, sep = ""))
##'
##' # Column binding
##' b1 <- matrix(1:9, nrow = 3, dimnames = list(1:3, letters[1:3]))
##' b2 <- matrix(10:18, nrow = 3, dimnames = list(4:6, letters[4:6]))
##' b3 <- matrix(19:27, nrow = 3, dimnames = list(7:9, letters[7:9]))
##'
##' qbind(paste("b", 1:3, sep = ""), type = "c")

qbind <- function(objects, type = c("r", "c")) {

  # Check arguments
  type <- match.arg(type)
  stopifnot(is.character(objects))

  # Verify objects exist
  for (o in objects) {
    if (!exists(o)) {
      stop("Object '", o, "' does not exist")
    }
  }

  # String with the command for binding
  stringToEval <- paste(if (type == "r") "r" else "c",
                        "bind(",
                        paste(objects, collapse = ", "),
                        ")",
                        sep = "")
  
  # Execute the binding
  return(eval(parse(text = stringToEval)))

} # qbind

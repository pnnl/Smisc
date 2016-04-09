##' Quickly row and column bind many objects together at once
##'
##' A wrapper for \code{\link{rbind}} and \code{\link{cbind}} to quickly bind
##' numerous objects together at once using a single call to \code{\link{rbind}} or
##'  \code{\link{cbind}}. This function is most helpful when there are
##' many objects to bind and the object names are easily represented in text.
##'
##' @export
##' @param objects A character vector with the names of the objects to be bound together
##'
##' @param type The type of binding, "row" for \code{\link{rbind}} or "col" for
##' \code{\link{cbind}}, and "c" for concatenating using \code{\link{c}}.  Defaults to
##' "row".
##'
##' @return The bound object
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
##' qbind(paste("b", 1:3, sep = ""), type = "col")
##'
##' # Concatenating a vector
##' a1 <- c(x = 1, y = 2)
##' a2 <- c(z = 3, w = 4)
##'
##' qbind(c("a1", "a2"), type = "c")

qbind <- function(objects, type = c("row", "col", "c")) {

  # Check arguments
  type <- match.arg(type)
  stopifnot(is.character(objects))

  # Verify objects exist
  for (o in objects) {
    if (!exists(o, envir = parent.frame())) {
      stop("Object '", o, "' does not exist")
    }
  }

  # String with the command for binding
  stringToEval <- paste(switch(type,
                               "row" = "rbind",
                               "col" = "cbind",
                               "c" = "c"),
                        "(",
                        paste(objects, collapse = ", "),
                        ")",
                        sep = "")

  # Execute the binding
  return(eval(parse(text = stringToEval), envir = parent.frame()))

} # qbind

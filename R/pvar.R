
##' Prints the name and value of one or more objects
##'
##' A convenience function for writing the names and values of objects to the
##' session window (and/or to another object).  Especially useful to keep track
##' of variables within loops.
##'
##' Input objects can be numeric, character, and/or logical.  They can also be
##' atomic or vectors.  It will accept data frames and matrices without error,
##' but the results won't be easily readable.
##'
##' @export
##' @param \dots Objects whose names and values are to be printed, separated by
##' commas. Can also be a simple list.
##'
##' @param digits Number of digits to display for numeric objects.  Defaults to
##' \code{NULL}, which corresponds to no restriction on the number of digits.  This
##' is passed to the \code{digits} argument of \code{\link{round}}.
##'
##' @param abbrev Number of characters to display for character objects.
##' Defaults to \code{NULL}, which corresonds to no restriction on the number
##' of characters.
##'
##' @param sep Character string that separates the objects that are printed
##'
##' @param verbose \code{=TRUE} writes the value of the object(s) to the
##' session window
##'
##' @return Invisibly returns a character string containing the names of the
##' objects and their values
##'
##' @author Landon Sego
##'
##' @keywords misc
##'
##' @examples
##' x <- 10
##' y <- 20.728923
##' z <- "This.long.string"
##'
##' pvar(x,y,z)
##' pvar(x,y,z, digits=2)
##' pvar(x,y,z, abbrev=4)
##' pvar(x,y,z, digits=2, abbrev=4)
##' pvar(x,y,z, sep = ",")
##'
##' # values can be vectors too
##' x <- 10:12
##' y <- c("This","That")
##' v2 <- pvar(x, y, verbose=FALSE)
##' v2
##'
##' # Or a simple list
##' pvar(list(x = 1:2, y = "this", z = TRUE))
##'
##' # Can be useful for keeping track of iterations in loops
##' for (i in 1:3) {
##'   for (j in letters[1:2]) {
##'     for (k in c("this","that")) {
##'       pvar(i, j, k)
##'     }
##'   }
##' }
##'
##'
pvar <- function(..., digits = NULL, abbrev = NULL, sep = ";", verbose = TRUE) {

  # Basic checks on arguments
  if (!is.null(digits))
    stopifnot(is.numeric(digits),
              length(digits) == 1)

  if (!is.null(abbrev))
    stopifnot(is.numeric(abbrev),
              length(abbrev) == 1)

  stopifnot(is.character(sep),
            length(sep) == 1,
            is.logical(verbose),
            length(verbose) == 1)


  # Grab the objects into a list
  vars <- list(...)
  vnames <- as.character(substitute(list(...)))[-1]

  # If a single list was provided
  if (length(vars) == 1) {
    if (is.list(vars[[1]])) {

      vars <- vars[[1]]
      vnames <- names(vars)

    }
  }

  # If an element of the list is NULL, replace it with a text string
  vars <- lapply(vars, function(x) {if (is.null(x))
                                      return("NULL")
                                    else
                                      return(x)})

  # Get length of the whole list
  len <- length(vars)

  # Make abbreviations
  if (!is.null(abbrev)) {
    for (i in 1:len) {
      if (is.character(vars[[i]]))
        vars[[i]] <- substr(vars[[i]], 1, abbrev)
    }
  }

  # Truncate to desired digits
  if (!is.null(digits)) {
    for (i in 1:len) {
      if (is.numeric(vars[[i]]))
        vars[[i]] <- round(vars[[i]], digits)
    }
  }

  # Collapse the text into a single string
  out <- paste(paste(vnames, lapply(vars, paste, collapse=", "),
                     sep = " = "), collapse = paste(sep, " ", sep = ""))

  if (verbose)
    cat(out, "\n")

  invisible(out)

} # end pvar()

##' Facilitate hard coding constants into R
##'
##' Hard coding isn't the best practice, but sometimes it's useful, especially in
##' on-off scripts for analyses.  This
##' function facilitate hard coding constants into R by printing the code from a
##' vector that would be needed to create that vector.
##'
##' @export
##' 
##' @param x A vector (numeric, character, or logical)
##'
##' @param vname A string indicating the name of the vector that will be "created" in the code
##'
##' @param vert A logical indicating whether the vector should be coded vertically (\code{TRUE}) or horizontally
##' (\code{FALSE})
##'
##' @param ... Additional arguments to \code{\link{cat}}
##'
##' @author Landon Sego
##'
##' @return Prints code (via \code{\link{cat}}) that will create the vector.  This code
##' can then be copied into other source code.  Returns nothing.
##'
##' @examples
##'# With characters
##'hardCode(letters[1])
##'hardCode(letters[1:3], vname = "new")
##'hardCode(letters[1], vert = FALSE)
##'hardCode(letters[1:3], vert = FALSE, vname = "new")
##'
##'# With numbers
##'hardCode(3:5)
##'hardCode(3:5, vert = FALSE, vname = "num")
##'
##'# With logicals
##'hardCode(TRUE)
##'hardCode(c(FALSE, TRUE), vert = FALSE)
##'hardCode(c(TRUE, FALSE, TRUE), vname = "newLogical")


hardCode <- function(x, vname = "x", vert = TRUE, ...) {

  # Basic checks
  stopifnot(is.vector(x),
            is.character(vname),
            length(vname) == 1,
            is.logical(vert),
            length(vert) == 1)

  # Switches for vertical or horizontal layout
  vs <- ifelse(rep(vert, 3),
               c("\n", paste(rep(" ", nchar(vname) + 6), collapse = ""), "\n"),
               c("", " ", ""))

  # Switches for including quotations for character strings
  qu <- ifelse(is.character(x), "\"", "")

  # Construct the ending portion of the string
  if (length(x) == 1) {
    beginString <- ""
    endString <- "\n"
  }
  else {
    beginString <- "c("
    endString <- paste(",", vs[1],
                       paste(paste(vs[2], qu, x[-1], sep = ""),
                             collapse = paste(qu, ",", vs[3], sep = "")),
                       qu, ")\n", sep = "")
  }

  # Write the final result
  cat(vname, " <- ", beginString, qu, x[1], qu, endString, sep = "", ...)

} # hardCode

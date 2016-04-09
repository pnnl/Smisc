##' Get the final set of characters after a single-character delimeter
##' 
##' Get the final set of characters from a vector after a single-character delimeter.  This can be useful in
##' filename manipulations, among other things.
##'
##' @export 
##' @inheritParams stripExtension
##'
##' @return Character vector of the strings that appear after the last instance of \code{split.char}
##' 
##' @author Landon Sego
##'
##' @seealso Additional functions for filename manipulations:  \code{\link{stripExtension}}, \code{\link{getExtension}},
##' \code{\link{stripPath}}, \code{\link{getPath}}, \code{\link{basename}}, \code{\link{dirname}}
##'
##' @keywords misc
##'
##' @examples
##' grabLast(c(a="email@@nowhere.com", "this.has.no.at.sign", "@@",
##'              "bad.email@@weird.com@@", NA, "2at's@@email@@good.net"), "@@")
grabLast <- function(vec, split.char) {

  # Check the split.char
  if ((length(split.char) != 1) | (!is.character(split.char)))
    stop("'split.char' argument must be a single character.\n")

  # Verify that 'vec' is a character vector
  if (!is.character(vec))
    stop("'", deparse(substitute(vec)), "' must be a character vector.\n")

  # Preserve the names for later
  names.vec <- names(vec)

  # If the last character is the split.char then flag it so that nothing will be returned
  check.for.ending.char <- function(x) {

     if (!is.na(x)) {
       n <- nchar(x)
       if (substr(x, n, n) == split.char)
         x <- paste(x, "BAD-ENDING-CHAR-NO-ONE-WOULD-DO-THIS", sep = "")
     }

     return(x)

  } # check.for.ending.char

  vec <- tapply(vec, 1:length(vec), check.for.ending.char)

  getLast <- function(v1) {

    # For NA's, nothing after the split.char, and ""
     if ((lv <- length(v1)) <= 1) {

       # For ""
       if (!length(v1))
         return("")
       # For NA
       if (is.na(v1))
         return(NA)
       # For nothing after the split.char
       else
         return("")
    }
    # If string doesn't end with the split.char
    else if (v1[lv] != "BAD-ENDING-CHAR-NO-ONE-WOULD-DO-THIS")
      return(v1[lv])
    # If it does end with the split.char
    else
      return("")

  } # getLast

  result <- unlist(lapply(strsplit(vec, split.char, fixed = TRUE), getLast))
  names(result) <- names.vec

  return(result)

} # grabLast


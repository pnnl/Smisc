##' Pad a vector of numbers with zeros
##'
##' Pad a numeric vector with zeros so that each element in the vector either
##' has 1) the same number of characters or 2) the same number of trailing
##' decimal places.
##'
##' For \code{side = 'left'}, \code{num} is the number of characters that each
##' element of the padded, output vector should have.  If \code{num = NULL}, the
##' largest number of characters that appears in the vector is chosen for
##' \code{num}.
##'
##' For \code{side = 'right'}, \code{num} is the number of decimal places to be
##' displayed. If \code{num = NULL}, the number of decimals in the element with
##' the largest number of decimal places is used.
##'
##' Note that \code{vec} must be numeric when \code{side = 'right'}.  However,
##' \code{vec} may be character when \code{side = 'left'}.
##'
##' @export
##' @param vec The numeric vector to be padded
##' 
##' @param num The maximum number of zeros that will be added. If \code{NULL}, the
##' value is chosen based on the longest string in the vector.
##' 
##' @param side The side to which the zeros are added.
##' 
##' @return Character vector with the leading (or trailing) elements padded
##' with zeros.
##' 
##' @author Landon Sego
##' 
##' @keywords misc
##' 
##' @examples
##' # Examples with 0's on the left
##' padZero(c(1, 10, 100))
##' padZero(c(1, 10, 100), num = 4)
##'
##' # Examples with 0's on the right
##' padZero(c(1.2, 1.34, 1.399), side = "r")
##' padZero(c(1.2, 1.34, 1.399), num = 5, side = "r")

padZero <- function(vec, num = NULL, side = c("left", "right")) {

  # Check arguments
  stopifnotMsg(# vec
               if (is.vector(vec) & (length(vec) >= 1)) {
                 is.numeric(vec) | is.character(vec) 
               } else FALSE,
               "'x' must be a numeric or character vector",

               # num
               if (!is.null(num)) {
                 if (is.numeric(num) & (length(num) == 1)) {
                    (num >= 0) & (num %% 1 == 0)
                 } else FALSE
               } else TRUE,
               "'num' must be NULL or a whole number: 1, 2, 3, ...")
    
  side <- match.arg(side)

  c.vec <- as.character(vec)
  nchar.vec <- nchar(c.vec)
  m.nchar.vec <- max(nchar.vec)

  if (side == "left") {

    if (is.null(num))
      num <- m.nchar.vec
    else if (num < m.nchar.vec)
      warning("'num = ", num, "' is less than the largest number of characters, ", m.nchar.vec)

    # Create the 0 vector
    zV <- tapply(nchar.vec, 1:length(nchar.vec), function(x) paste(rep(0, max(0, num - x)), collapse = ""))

    out <- paste(zV, c.vec, sep="")

  } # if padding on the left

  # padding on the right
  else {

    if (!is.numeric(vec))
      stop("'vec' should be numeric when \"side = 'right'\"")

    # Grab the decimal portion of the vector and identify the maximum length
    m.dec.length <- max(nchar(getExtension(c.vec)))

    if (is.null(num))
      num <- m.dec.length
    else if (num < m.dec.length)
      warning("num = ", num, " is less than the largest number of decimals, ", m.dec.length,
              ", so rounding will occur")

    out <- sprintf(paste("%.", num, "f", sep=""), vec)

  } # else

  return(out)

} # padZero

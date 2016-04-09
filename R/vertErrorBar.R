##' Draw vertical error bar(s) on a plot
##'
##' @details Buyer beware!  It's up to the user to determine what the statistically correct length
##' of the error bar should be.
##'
##' Either \code{center} and \code{barLength} must be specified, or \code{min.y} and
##' \code{max.y} must be specifed.
##'
##' Note that \code{width}, \code{center}, \code{barLength}, \code{min.y}, \code{max.y}, 
##' and \code{blankMiddle} should be \code{NULL}, numeric values of length 1, or numeric values with
##' the same length as \code{x}.  If they have the same length of \code{x}, the bars can have
##' different vertical lengths, widths, and \code{blankMiddle} values if desired.
##'
##' @export
##' @param x Vector of x value on the plot around which the vertical error bar will be drawn
##'
##' @param width The total width of the cross hatches on top and bottom of the bars, can
##' be a vector or a single value
##'
##' @param center Vector of values designating the vertical center of the error bars, can
##' be a vector or a single value
##'
##' @param barLength The total vertical length of the bars, can be a vector or a single value
##'
##' @param min.y Vector of values indicating the vertical bottoms of the bars
##'
##' @param max.y Vector of values indicating the vertical tops of the bars
##'
##' @param blankMiddle the vertical length of a blank spot that will be produced in the middle of
##' the error bar. This is useful when the bar is placed around symbols (so as not to overwrite them).
##' Defaults to \code{NULL}, in which case a solid error bar is drawn. Can also be a vector or a
##' single value.
##'
##' @param \dots additional arguments to \code{\link{lines}}.
##'
##' @author Landon Sego
##'
##' @return Nothing is returned, the error bar(s) is/are drawn on the plot
##'
##' @examples
##' set.seed(343)
##'
##' # Make a plot of some standard normal observations
##' x <- 1:9
##' y <- rnorm(9)
##' 
##' plot(x, y, pch = as.character(1:9), ylim = c(-2, 2) + range(y),
##'      ylab = "Z", xlab = "Indexes")
##'
##' # Draw the error bars
##' vertErrorBar(x, 0.3, center = y, barLength = 2 * 1.96, blankMiddle = 0.25)

vertErrorBar <- function(x, width, center = NULL, barLength = NULL,
                         min.y = NULL, max.y = NULL, blankMiddle = NULL, ...) {

  # Function that checks for numeric
  cNum <- function(var) {
    if (!is.null(var)) is.numeric(var) else TRUE
  }
  # Function that produces the error message
  cMsg <- function(var) {
    paste("'", var, "' must be numeric or NULL", sep = "") 
  }
    
  # Type checks
  stopifnotMsg(is.numeric(x), "'x' must be numeric",
               is.numeric(width), "'width' must be numeric",
               cNum(center), cMsg("center"),
               cNum(barLength), cMsg("barLength"),
               cNum(min.y), cMsg("min.y"),
               cNum(max.y), cMsg("max.y"),
               cNum(blankMiddle), cMsg("blankMiddle"))

  # Function that checks for length
  cLen <- function(var) {
    if (!is.null(var)) {
      (length(var) == 1) | (length(var) == length(x))
    }
    else TRUE
  }
  # Function that produces the error message
  cMsg <- function(var) {
    paste("Length of '", var, "' must be 1 or the length of 'x'", sep = "") 
  }

  # Checks for length
  stopifnotMsg(cLen(width), cMsg("width"),
               cLen(center), cMsg("center"),
               cLen(barLength), cMsg("barLength"),
               cLen(min.y), cMsg("min.y"),
               cLen(max.y), cMsg("max.y"),
               cLen(blankMiddle), cMsg("blankMiddle"))
  
  # Logical checks
  cenBarLength <- as.numeric(!is.null(center)) + as.numeric(!is.null(barLength))
  if (cenBarLength == 1) {
    stop("'center' and 'barLength' must be specified together")
  }

  minMax <- as.numeric(!is.null(min.y)) + as.numeric(!is.null(max.y))
  if (minMax == 1) {
    stop("'min.y' and 'max.y' must be specified together")
  }

  if ((cenBarLength == 0) & (minMax == 0)) {
    stop("Either 'center' and 'barLength' must be specified, or 'min.y' and 'max.y' must be specified")
  }
  else if ((cenBarLength == 2) & (minMax == 2)) {
    warning("'center', 'barLength', 'min.y', and 'max.y' were all specified. 'min.y' and 'max.y' will be ignored")
  }

  # Convert min.y and max.y to and y as needed
  if (cenBarLength == 0) {

    stopifnotMsg(all(min.y <= max.y),
                 "All elements of 'min.y' must be less than or equal to the corresponding elements of 'max.y'")

    # Get the midpoints
    center <- (min.y + max.y) / 2

    # Get the barLength
    barLength <- max.y - min.y

  }

  # Last checks
  stopifnotMsg(all(width >= 0),
               "all values of 'width' must be non-negative",
               all(barLength >= 0),
               "all values of 'barLength' must be non-negative")

  # Get the length
  n <- length(x)

  # Replicate if necessary
  if (length(barLength) == 1) {
    barLength <- rep(barLength, n)
  }
  if (length(width) == 1) {
    width <- rep(width, n)
  }

  # Make the vertical line
  if (is.null(blankMiddle)) {

    for (i in 1:n) {
      lines(rep(x[i], 2), c(center[i] - barLength[i] / 2, center[i] + barLength[i] / 2), ...)
    }
    
  }

  else {

    # Replicate if necessary
    if (length(blankMiddle) == 1) {
      blankMiddle <- rep(blankMiddle, n)
    }

    # Add in lines to create an empty space around plotted points
    for (i in 1:n) {
      lines(rep(x[i], 2), c(center[i] - barLength[i] / 2, center[i] - min(barLength[i] / 2, blankMiddle[i] / 2)), ...)
      lines(rep(x[i], 2), c(center[i] + barLength[i] / 2, center[i] + min(blankMiddle[i] / 2, barLength[i] / 2)), ...)
    }
  }

  # Make the cross hatches
  for (i in 1:n) {
    lines(c(x[i] - width[i] / 2, x[i] + width[i] / 2), rep(center[i] + barLength[i] / 2, 2), ...)
    lines(c(x[i] - width[i] / 2, x[i] + width[i] / 2), rep(center[i] - barLength[i] / 2, 2), ...)
  }

  return(invisible())

} # vertErrorBar

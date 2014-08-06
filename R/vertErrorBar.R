##' Vertical error bar drawing function
##'
##' Vertical error bar drawing function
##'
##' Buyer beware!  It's up to the user to determine what the statistically correct height
##' of theerror bar should be.
##'
##' Note that \code{x}, \code{y} should be the same length.  \code{height}, \code{width},
##' and \code{blankMiddle} should either be numeric values of length 1, or the same length
##' as \code{x}, in which case the bars could have
##' different heights, widths, and blankMiddle values if desired.
##'
##' Either \code{center} and \code{height} must be specified, or \code{min.y} and
##' \code{max.y} must be specifed.
##'
##' @export
##' @param x Vector of x value on the plot around which the vertical error bar will be drawn
##' @param width The total width of the cross hatches on top and bottom of the bars, can
##' be a vector or a single value
##' @param center Vector of values designating the vertical center of the error bars, can
##' be a vector or a single value
##' @param height The total height of the bars, can be a vector or a single value
##' @param min.y Vector of values indicating the vertical bottoms of the bars
##' @param max.y Vector of values indicating the vertical tops of the bars
##' @param blankMiddle the height of a blank spot that will be produced in the middle of
##' the error bar.
##'   This is useful when the bar is placed around symbols (so as not to overwrite them).
##' Defaults to
##'   \code{NULL}, in which case a solid error bar is drawn. Can also be a vector or a
##' single value.
##' @param ... additional arguments to \code{\link{lines}}.
##' @author Landon Sego
##' @return Nothing is returned, the error bar is drawn on the plot
##'
##' @examples
##' set.seed(343)
##'
##' # Make a plot
##' plot(x <- 1:10, y <- rnorm(10), pch = as.character(1:10), ylim = 2.5 * range(y),
##'      ylab = "Z", xlab = "Indexes")
##'
##' # Draw the error bars
##' vertErrorBar(x, 0.3, center = y, height = 1.96, blankMiddle = 0.25)
vertErrorBar <- function(x, width, center = NULL, height = NULL,
                         min.y = NULL, max.y = NULL, blankMiddle = NULL, ...) {

  # Logical checks
  cenHeight <- as.numeric(!is.null(center)) + as.numeric(!is.null(height))
  if (cenHeight == 1)
    stop("'center' and 'height' must be specified together")

  minMax <- as.numeric(!is.null(min.y)) + as.numeric(!is.null(max.y))
  if (minMax == 1)
    stop("'min.y' and 'max.y' must be specified together")

  if ((cenHeight == 0) & (minMax == 0)) {
    stop("Either 'center' and 'height' must be specified, or 'min.y' and 'max.y' must be specified")
  }
  else if ((cenHeight == 2) & (minMax == 2)) {
    warning("'center', 'height', 'min.y', and 'max.y' were all specified. 'min.y' and 'max.y' will be ignored")
  }

  # Convert min.y and max.y to and y as needed>
  if (cenHeight == 0) {

    stopifnot(all(min.y <= max.y))

    # Get the midpoints
    center <- (min.y + max.y) / 2

    # Get the height
    height <- max.y - min.y

  }


  # Checks
  stopifnot(length(x) == length(center),
            (length(height) == 1) | (length(height) == length(x)),
            (length(width) == 1) | (length(width) == length(x)),
            all(width >= 0),
            all(height >= 0))


  # Get the length
  n <- length(x)

  # Replicate if necessary
  if (length(height) == 1)
    height <- rep(height, n)
  if (length(width) == 1)
    width <- rep(width, n)

  # Make the vertical line
  if (is.null(blankMiddle)) {

    for (i in 1:n)
      lines(rep(x[i], 2), c(center[i] - height[i] / 2, center[i] + height[i] / 2), ...)
  }

  else {
    # Check
    stopifnot((length(blankMiddle) == 1) | (length(blankMiddle) == length(x)))

    # Replicate if necessary
    if (length(blankMiddle) == 1)
      blankMiddle <- rep(blankMiddle, n)

    # Add in lines to create an empty space around plotted points
    for (i in 1:n) {
      lines(rep(x[i], 2), c(center[i] - height[i] / 2, center[i] - min(height[i] / 2, blankMiddle[i] / 2)), ...)
      lines(rep(x[i], 2), c(center[i] + height[i] / 2, center[i] + min(blankMiddle[i] / 2, height[i] / 2)), ...)
    }
  }


  # Make the cross hatches
  for (i in 1:n) {
    lines(c(x[i] - width[i] / 2, x[i] + width[i] / 2), rep(center[i] + height[i] / 2, 2), ...)
    lines(c(x[i] - width[i] / 2, x[i] + width[i] / 2), rep(center[i] - height[i] / 2, 2), ...)
  }

  return(invisible())

} # errorBar

##' Calculate the moving average using a 2-sided, symmetric window
##'
##' Wrapper for \code{smartFilter} that creates a set of symmetric weights for
##' the 2-sided window based on the Gaussian kernel, exponetial decay, linear
##' decay, or simple uniform weights, and then calculates the moving average
##' using those weights.
##'
##' All the weights are normalized (so that they sum to 1) prior to calculating
##' the moving average.
##'
##' Since it uses \code{\link{smartFilter}} to calculate the moving average,
##' the moving average for points near the edge of the series or in the
##' neighborhood of missing values are calculated using as much of the window
##' weights as possible.
##'
##' @export
##' @param y The numerical vector for which the moving averages will be
##' calculated.  If \code{NULL}, the moving averages are not calculated--but this can
##' be useful for visualizing the weights. See \code{plot}.
##' 
##' @param bw 'bandwidth', which is roughly half the width of the moving
##' window.  The total width of the window is \code{2 * bw + 1}.
##' 
##' @param type Character string which uniquely indentifies the type of weights
##' to use, corresponding to the Gaussian kernel, exponential decay, linear
##' decay, or uniform weights. Defaults to 'gaussian'.
##' 
##' @param furthest.weight The unormalized value of the weights at the left and
##' right edges of the window
##' 
##' @param center.weight The unnormalized value of the weights at the center of
##' the window
##' 
##' @param plot \code{ = TRUE} produces a plot which shows the weights.
##' 
##' @param \dots additional arguments to \code{\link{smartFilter}}
##' 
##' @return If \code{y} is not \code{NULL}, returns the moving average of the
##' series.  Also produces a plot of the weights if \code{plot = TRUE}.
##' 
##' @author Landon Sego
##' 
##' @seealso \code{\link{smartFilter}}, \code{\link{filter}}
##' 
##' @keywords misc
##' 
##' @examples
##' x <- rnorm(25)
##' movAvg2(x, bw = 10, type = "e", center.weight = 2)
##'
##' # Note how it produces the same values as filter (except at the edge
##' # of the series
##' x <- rnorm(10)
##' movAvg2(x, bw = 2, type = "u")
##' filter(x, rep(1,5)/5)
##'
##' # These are also the same, except at the edge.
##' movAvg2(x, bw = 1, type = "l", furthest.weight = 0.5, center.weight = 1)
##' filter(x, c(0.5, 1, 0.5)/2)

movAvg2 <- function(y = NULL, bw = 30, type = c("gaussian", "exponential", "linear", "uniform"),
                    furthest.weight = 0.01, center.weight = 1, plot = FALSE, ...) {

  type <- match.arg(type)

  if (type == "gaussian") {
    b <- (bw^2)/log(furthest.weight / center.weight)
    wts <- center.weight * exp((-bw:bw)^2 / b)
  }

  else if (type == "exponential") {
    b <- bw / log(furthest.weight / center.weight)
    wts <- center.weight * exp(abs(-bw:bw) / b)
  }

  else if (type == "linear")
    wts <- (furthest.weight - center.weight) * abs(-bw:bw) / bw + center.weight

  else
    wts <- rep(1, 2 * bw + 1)

  if (any(wts < 0))
    warning("Some of the window weights are < 0.\n")

  # Plot the unnormalized weights
  if (plot) {
    plot(-bw:bw, wts,
         xlim = c(-(bw+3),(bw+3)),
         ylim = c(0, max(wts)),
         lwd = 2,
         col = "Blue",
         type = "b", xlab = "Data Index",
         ylab = "Window weights",
         main = "Unormalized weights for moving window",
         font.main = 1)
    abline(h = 0)
  }

  # Calculate the moving average using smartFilter and normalized weights
  if (!is.null(y))
    return(smartFilter(y, wts / sum(wts), ...))

} # gaussMovAvg

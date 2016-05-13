##' Calculate the moving average using a 2-sided, symmetric window
##'
##' Wrapper for \code{\link{smartFilter}} that creates a set of symmetric weights for
##' the 2-sided window based on the Gaussian kernel, exponential decay, linear
##' decay, or simple uniform weights, and then calculates the moving average (dot product)
##' using those weights.
##'
##' All the weights are normalized (so that they sum to 1) prior to calculating
##' the moving average.  The moving "average" is really the moving dot product of the
##' normalized weights and the corresponding elements of \code{y}.
##'
##' Since it uses \code{\link{smartFilter}} to calculate the moving average,
##' the moving average for points near the edge of the series or in the
##' neighborhood of missing values are calculated using as much of the window
##' weights as possible.
##'
##' @export
##' @param y The numerical vector for which the moving averages will be
##' calculated.  If \code{NULL}, the moving averages are not calculated, but the
##' weights are calculated and included in the attributes of the returned object.
##'
##' @param bw A single, positive whole number that indicates the 'bandwidth' of the window,
##' which is roughly half the width of the moving window.  The total width of the window is \code{2 * bw + 1}.
##'
##' @param type Character string which uniquely indentifies the type of weights
##' to use, corresponding to the Gaussian kernel, exponential decay, linear
##' decay, or uniform weights. Defaults to \code{gaussian}.
##'
##' @param furthest.weight A single, positive number corresponding to the unormalized value of
##' the weights at the left and right edges of the window.  Ignored when \code{type = 'uniform'}.
##'
##' @param center.weight A single, positive number corresponding to the unnormalized value of
##' the weights at the center of the window.
##'
##' @param x Object of class \code{movAvg2}.
##'
##' @param \dots For \code{movAvg2}, these are additional arguments to \code{\link{smartFilter}}.
##' For the \code{print} and \code{plot} methods, the "\dots" are additional arguments passed to
##' \code{\link{print.default}} and \code{\link{plot.default}}, respectively.
##'
##' @return An object class \code{movAvg2}, which is a numeric vector containing the moving average (dot product) of the
##' series, with attributes that describe the weights.  If \code{y = NULL}, \code{numeric(0)} is returned.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{smartFilter}}, \code{\link{filter}}
##'
##' @keywords misc
##'
##' @examples
##' z <- movAvg2(rnorm(25), bw = 10, type = "e", center.weight = 2)
##' z
##'
##' # Look at the attributes
##' attributes(z)
##'
##' # Plot the weights
##' plot(z)
##'
##' # If we just want to see the weights (without supplying data)
##' plot(movAvg2(bw = 20, type = "g", center.weight = 1))
##'
##' # Note how it produces the same values as filter (except at the edge
##' # of the series
##' x <- rnorm(10)
##' movAvg2(x, bw = 2, type = "u")
##' filter(x, rep(1, 5) / 5)
##'
##' # These are also the same, except at the edge.
##' movAvg2(x, bw = 1, type = "l", furthest.weight = 0.5, center.weight = 1)
##' filter(x, c(0.5, 1, 0.5) / 2)

movAvg2 <- function(y = NULL, bw = 30, type = c("gaussian", "exponential", "linear", "uniform"),
                    furthest.weight = 0.01, center.weight = 1, ...) {

  # Check arguments

  stopifnotMsg(# y
               if (!is.null(y)) {
                 (length(y) > 0) & is.numeric(y)
               } else TRUE,
               "'y' must be NULL or a numeric vector with at least one element",

               # bw
               if (is.numeric(bw) & (length(bw) == 1)) {
                 (bw %% 1 == 0) & (bw > 0)
               } else FALSE,
               "'bw' must be a single, positive integer: 1, 2, 3, ...",

               # furthest.weight
               if (is.numeric(furthest.weight) & (length(furthest.weight) == 1)) {
                 furthest.weight > 0
               } else FALSE,
               "'furthest.weight' must be a single, positive number",

               # center.weight
               if (is.numeric(center.weight) & (length(center.weight) == 1)) {
                 center.weight > 0
               } else FALSE,
               "'center.weight' must be a single, positive number")

  type <- match.arg(type)

  # Define the weights
  if (type == "gaussian") {
    b <- (bw^2) / log(furthest.weight / center.weight)
    wts <- center.weight * exp((-bw:bw)^2 / b)
  }

  else if (type == "exponential") {
    b <- bw / log(furthest.weight / center.weight)
    wts <- center.weight * exp(abs(-bw:bw) / b)
  }

  else if (type == "linear") {
    wts <- (furthest.weight - center.weight) * abs(-bw:bw) / bw + center.weight
  }

  else {
    wts <- rep(center.weight, 2 * bw + 1)
  }

  if (any(wts < 0)) {
    stop("This shouldn't have happened: some of the window weights are < 0")
  }


  # Calculate the moving average using smartFilter and normalized weights
  if (!is.null(y)) {
    out <- smartFilter(y, wts / sum(wts), ...)
  }
  else {
    out <- numeric(0)
  }

  # Add the class
  class(out) <- c("movAvg2", class(out))

  # Add attributes (the weights, type, and parameters)
  attributes(out) <- c(attributes(out),
                       list(type = type,
                            bw = bw,
                            furthest.weight = furthest.weight,
                            center.weight = center.weight,
                            wts = wts))

  # Return result
  return(out)

} # movAvg2

##' @method print movAvg2
##'
##' @describeIn movAvg2 Prints the \code{movAvg2} object by only showing the series of dot products and suppressing the attributes.
##'
##' @export

print.movAvg2 <- function(x, ...) {

  printWithoutAttributes(x, ...)

} # print.movAvg2

##' @method plot movAvg2
##'
##' @describeIn movAvg2 Plots the unnormalized weights.
##'
##' @export

plot.movAvg2 <- function(x, ...) {

  bw <- attributes(x)$bw
  wts <- attributes(x)$wts
  type <- attributes(x)$type

  # Default arguments
  defaultArgs <- list(x = -bw:bw,
                      y = wts,
                      xlim = c(-(bw + 3), (bw + 3)),
                      ylim = c(0, max(wts)),
                      lwd = 2,
                      col = "Blue",
                      type = "b",
                      xlab = "Data Index",
                      ylab = "Window Weights",
                      main = sub("gauss", "Gauss", paste("Unnormalized", type, "weights")),
                      font.main = 1)

  # Make the plot
  do.call(plot, blendArgs(defaultArgs, ...))

  # Add in a horiztonal line at the bottom
  abline(h = 0)

} # plot.movAvg2




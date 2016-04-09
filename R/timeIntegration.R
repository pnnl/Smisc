# Integrate a parameter over time by just calculating the area of the trapezoids under the curve

########  Returns the results in terms of hours  ########################################

# How does this do with negative data?       Good
# How does it do with a constant function?   Good

# Midpoint algorithm validated on 9/21/09 by Landon Sego



##' Approximate the integral of a vector of data over time
##'
##' Integrate a series over time by calculating the area under the "curve" of
##' the linear interpolation of the series (akin to the Trapezoid rule).
##' This is especially useful in calculating
##' energy usage: kilowatt-hours, watt-seconds, etc.
##'
##' If \code{upper} or \code{lower} does not correspond to a data point, a
##' linear interpolation is made between the two neighboring time points to
##' predict the resulting data value.
##'
##' @export
##' @param data Vector of numerical data
##'
##' @param time Vector of timestamps which correspond to \code{data}. These can
##' either character or POSIXct.
##'
##' @param lower The time (character or POSIXct) of the lower bound of the
##' integration
##'
##' @param upper The time (character or POSIXct) of the upper bound of the
##' integration
##'
##' @param check.plot \code{=TRUE} makes a plot which illustrates the
##' integration.
##'
##' @param units The units of integration, defaults to hours.  It is only
##' required to supply enough characters to uniquely complete the name.
##'
##' @return The approximation of the integral by joining the points in the
##' series in a linear fashion and calculating the area under this "curve". 
##'
##' @author Landon Sego
##'
##' @keywords misc
##'
##' @examples
##'# Some example power data
##'data(PowerData)
##'
##'par(mfrow = c(2, 1))
##'
##'# Calculate the kilowatt-minutes, display graph which shows how the
##'# integration is done.  This example calculates the integral using
##'# a contiguous subset of the data
##'int1 <- timeIntegration(PowerData,
##'                        # Convert to POSIXct in order to subtract time
##'                        lower = "5/6/2008 17:00:09",
##'                        upper = "5/6/2008 17:01:36",
##'                        check.plot = TRUE, units = "m")
##' 
##'# This example calculates the integral for all the data in 'powerData'
##'int2 <- timeIntegration(PowerData, check.plot = TRUE, units = "m")
##'
##'# Print the outcome
##'pvar(int1, int2)

timeIntegration <- function(data, time = names(data), lower = time[1], upper = time[length(time)],
                            check.plot = FALSE, units = c("hours", "minutes", "seconds")) {

  units <- match.arg(units)

  if (is.null(time))
    stop("'data' must either have timestamps in the names, or 'time' must be supplied explicitly")

  # Assume all values are non-negative
  if (!all(data > 0))
    stop("All the values of data must be non-negative")

  if (length(time) != length(data))
    stop("length(time) != length(data)")

  # Convert the data to a vector
  data <- as.vector(data)

  # If only one data point, the integral must be 0
  if (length(data) == 1) {
    warning("Only 1 data point was provided\n")
    return(0)
  }

  # Check time
  if (!all(class(time) %in% c("POSIXt","POSIXct"))) {
    if (is.character(time))
      time <- formatDT(time)$dt.posix
    else
      stop("'time' was not posix or character\n")
  }

  # Check lower
  if (!all(class(lower) %in% c("POSIXt","POSIXct"))) {
    if (is.character(lower))
      lower <- formatDT(lower)$dt.posix
    else
      stop("'lower' was not posix or character\n")
  }

  # Check upper
  if (!all(class(upper) %in% c("POSIXt","POSIXct"))) {
    if (is.character(upper))
      upper <- formatDT(upper)$dt.posix
    else
      stop("'upper' was not posix or character\n")
  }

  # [lower, upper] needs to be contained in time
  if (!((min(time) <= lower) & (upper <= max(time))))
    stop("[lower, upper] needs to be contained in range(time)\n")

  # lower needs to be less than upper
  if (lower > upper)
    stop("'lower' must be less than or equal to 'upper'\n")

  # time needs to be strictly increasing
  if (!all(diff(as.numeric(time)) > 0))
    stop("'time' must be strictly increasing\n")

  # Window of indexes for data that will be integrated
  window <- (lower < time) & (time < upper)

  # Find linear interpolations for the endpoints
#  nd <- approx.irts(irts(time, data), c(lower, time[window], upper), rule=1, method="linear")
  nd <- approx(time, data, c(lower, time[window], upper), rule = 1, method = "linear")
  names(nd) <- c("time", "value")


  # Simply calculates the exact area under the curve by finding the height of the midpoint on
  # each interval and then calculating the area of the trapezoid
  n <- length(nd$time)
  time.numeric <- as.numeric(nd$time)
  deltas <- diff(time.numeric)

  time.numeric.midpoints <- time.numeric[1:(n-1)] + deltas / 2
  midpoint.heights <- approx(as.numeric(time), data, time.numeric.midpoints)$y

  # pvar(time.numeric, time.numeric.midpoints, midpoint.heights, deltas)


  if (check.plot) {

    op <- par(mar = c(3, 3, 5, 0.5))

    ry <- diff(range(data))
    
    plot(time, data, type = "b", font.main = 1, cex.main = 0.9,
         ylim = ry * c(-0.05, 0.05) + range(data),
         main = paste("Black open circles:  Data values\n",
                      "Blue dots: Midpoints for each trapezoid\n",
                      "Red lines: Vertical edges of trapezoids\n",
                      "The integral is the area of the trapezoids"),
         axes = FALSE, frame = TRUE)

    ## For some reason, this was hanging the plots completely
    ## polygon(c(nd$time[1], nd$time, nd$time[length(nd$time)]),
    ##         c(0, nd$value, 0),
    ##         density = 10, col = "Green")

    lines(time, data, type = "b")

    # Trapezoid lines
    for (i in 1:length(nd$time)) {
      lines(rep(nd$time[i], 2), c(0, nd$value[i]), col = "Red")
    }


    points(time.numeric.midpoints, midpoint.heights, col = "Blue", pch = 19, cex = 1.2)

    axis(2)
    smartTimeAxis(time, time.format = "hh:mm:ss")

    par(op)

  }

  # Define the divisor
  div <- switch(units, hours = 3600, minutes = 60, seconds = 1)

  # Divide by 3600 since deltas are in seconds
  return(as.vector(deltas %*% midpoint.heights) / div)

} # timeIntegration


##' Produces a time axis with smart spacing
##'
##' Produces a time axis on a plot with interval spacing units that are
##' intuitive.  It is intended to be applied to periods of time that do not
##' exceed 24 hours (i.e. it does not produce a date stamp in the time axis).
##'
##' @details \code{smartTimeAxis} attempts to choose a "natural" spacing for the time axis ticks that results in the
##' number of ticks being as close as possible to 
##' \code{nticks}.  Possibilities for natural spacings include 1, 5, 10,
##' 15 seconds, etc., or 1, 2, 5, 10, minutes etc., or 0.5, 1, 1.5 hours, etc.
##'
##' @export
##' @param time.vec A time object (vector) that was used to construct the plot,
##' presumed to be ordered chronologically
##'
##' @param nticks The target number of ticks to use in the axis
##'
##' @param side Same as the \code{side} argument in \code{\link{axis}}
##'
##' @param time.format Character string indicting the time format to display on the axis. The choices are displayed in
##' the Usage. Defaults to \code{hh:mm}.
##'
##' @return Places the axis on the plot.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{axis.POSIXct}}
##'
##' @keywords misc
##'
##' @examples
##' # Get data and set the options to the horizontal axis labels will be
##' # oriented vertically
##' data(timeData)
##' op <- par(las = 2, mfrow = c(3, 1), mar = c(4, 4, 2, 0.5))
##'
##' # Make the default plot
##' plot(timeData, xlab = "", main = "Default intervals")
##'
##' # Make the plot with specialized time axis
##' plot(timeData, axes = FALSE, frame.plot = TRUE, xlab = "", main = "10 minute intervals")
##'
##' # Add y-axis
##' axis(2)
##'
##' # Add the time axis
##' smartTimeAxis(timeData$time, nticks = 10)
##'
##' # Only look at a small portion of the data with a different time format
##' par(mar = c(7, 4, 2, 0.5))
##' 
##' plot(timeData[200:237,], type = "b", axes = FALSE, frame.plot = TRUE,
##'      xlab = "", main = "15 second intervals")
##' 
##' axis(2)
##' 
##' smartTimeAxis(timeData[200:237,"time"], nticks = 20, time.format = "hh:mm:ss pm")
##'
##' # Restore the original par settings
##' par(op)

smartTimeAxis <- function(time.vec, nticks = 15, side = 1,
                          time.format = c("hh:mm",
                                          "hh:mm:sspm",
                                          "hh:mm:ss pm",
                                          "hh:mm:ss",
                                          "hh:mmpm",
                                          "hh:mm pm")) {
                              

  # Check arguments
  stopifnotMsg(inherits(time.vec, "POSIXct") & (length(time.vec) > 1),
               "'time.vec' must be a 'POSIXct' vector of time and have at least two elements",
               if (is.numeric(nticks) & (length(nticks) == 1)) {
                 (nticks > 0) & (nticks %% 1 == 0)
               } else FALSE,
               "'nticks' must be a positive whole number",
               side %in% c(1, 2, 3, 4),
               "'side' must be on of 1, 2, 3, or 4")

  # Match the args for time.format
  time.format <- match.arg(time.format)

  # Select the time format
  time.format <- switch(time.format,
                        "hh:mm"       = "%H:%M",
                        "hh:mm:sspm"  = "%I:%M:%S%p",
                        "hh:mm:ss pm" = "%I:%M:%S %p",
                        "hh:mm:ss"    = "%H:%M:%S",
                        "hh:mmpm"     = "%I:%M%p",
                        "hh:mm pm"    = "%I:%M %p")

  ################################################################################
  # Determine the ideal spacing for the time
  ################################################################################

  # Number of seconds in the time range
  time.range <- as.numeric(difftime(time.vec[length(time.vec)], time.vec[1], units = "s"))

  # If the range is longer than 24 hours
  if (time.range > 86400) {
    warning("The time range is > 24 hours.  The resulting time axis labeling may not be appropriate.\n")
  }

  # possible spacings
  possible.spacings <- c(1/60, 5/60, 10/60, 15/60, 30/60, 45/60,  # Seconds within a minute
                         1, 2, 5, 10, 20, 30, 45,  # Minutes within the hour
                         seq(60, 600, by = 30)) * 60  # 1 to 10 hours

  possible.n.ticks <- time.range / possible.spacings
  adiff <- abs(possible.n.ticks - nticks)
  ideal.spacing <- possible.spacings[which(adiff == min(adiff))]

  # Grab the first one in case there are multiple matches
  ideal.spacing <- ideal.spacing[1]

  # Identify the ideal starting point

  # Nearest 1 second
  if (ideal.spacing == 1) 
    mod.factor <- 1

  # Find nearest 5 second mark for 5, 10, or 15 second spacing
  else if (ideal.spacing <= 15)
    mod.factor <- 5

  # Find nearest 15 second mark for 30, 45 second spacing
  else if (ideal.spacing <= 45)
    mod.factor <- 15

  # Find nearest 1 minute for 1 minute spacing
  else if (ideal.spacing == (1 * 60))
    mod.factor <- 1 * 60

  # Find nearest even minute for 2 minute spacing
  else if (ideal.spacing == (2 * 60))
    mod.factor <- 2 * 60

  # Find the nearest 5 minute for 5, 10 minute spacings
  else if (ideal.spacing <= (10 * 60))
    mod.factor <- 5 * 60

  # Find the nearest 10 minute for 20 minute spacings
  else if (ideal.spacing == (20 * 60))
    mod.factor <- 10 * 60

  # Find the nearest 15 minute for 30, 45 minute spacings
  else if (ideal.spacing <= (45 * 60))
    mod.factor <- 15 * 60

  # Find the nearest 30 minute for all spacings 1 hour and larger
  else
    mod.factor <- 30 * 60

  # Calculate the starting point
  start <- min(time.vec) + as.numeric(as.logical(mod.factor)) * (mod.factor - as.numeric(min(time.vec)) %% mod.factor)

  ################################################################################
  # Make the time axis
  ################################################################################
  axis.POSIXct(side, at = seq(start, max(time.vec), by = ideal.spacing),
               format = time.format)

} # smartTimeAxis

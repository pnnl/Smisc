##' Produces a time axis with smart spacing
##'
##' Produces a time axis on a plot with interval spacing units that are
##' intuitive.  It is intended to be applied to periods of time that do not
##' exceed 24 hours (i.e. it does not produce a date stamp in the time axis).
##'
##' Chooses a "natural" spacing for the time axis ticks that gives closest to
##' \code{nticks} ticks.  Possibilities for natural spacings include 1, 5, 10,
##' 15 seconds, etc. OR 1, 2, 5, 10, minutes etc., OR 0.5, 1, 1.5 hours, etc.
##'
##' @export
##'
##' @param time.vec A time object (vector) that was used to construct the plot,
##' presumed to be ordered chronologically
##' @param nticks The target number of ticks to use in the axis
##' @param time.format Same as the \code{time.outformat} argument in
##' \code{\link{formatDT}}
##' @param side Same as the \code{side} argument in \code{\link{axis}}
##' @return Places the axis on the plot.
##' @author Landon Sego
##' @seealso \code{\link{axis.POSIXct}}
##' @keywords misc
##' @examples
##'
##' # Get data and set the options to the horizontal axis labels will be
##' # oriented vertically
##' data(timeData)
##' op <- par(las=2, mfrow=c(3,1), mar=c(4,4,1,0.5))
##'
##' # Make the default plot
##' plot(timeData, xlab="")
##'
##' # Make the plot with specialized time axis
##' plot(timeData, axes=FALSE, frame.plot=TRUE, xlab="")
##'
##' # Add y-axis
##' axis(2)
##'
##' # Add the time axis
##' smartTimeAxis(timeData$time, nticks=10)
##'
##' # Only look at a small portion of the data with a different time format
##' par(mar=c(7,4,1,0.5))
##' plot(timeData[200:237,], type="b", axes=FALSE, frame.plot=TRUE, xlab="")
##' axis(2)
##' smartTimeAxis(timeData[200:237,"time"], nticks=20, time.format="hh:mm:ss pm")
##'
##' # Restore the original par settings
##' par(op)
##'
##'
smartTimeAxis <- function(time.vec, nticks=15, time.format="hh:mm", side=1) {

  # time.vec -- a POSIXct vector
  # nticks -- the target number of ticks to create
  # time.format -- the time format, see 'formatDT'
  # side -- see 'axis'

  if (length(time.vec) <= 1)
    stop("'time.vec' must be a vector with at least 2 elements\n")

  # Select the time format
       if (tolower(time.format)=="hh:mm:sspm")  time.format <- "%I:%M:%S%p"
  else if (tolower(time.format)=="hh:mm:ss pm") time.format <- "%I:%M:%S %p"
  else if (tolower(time.format)=="hh:mm:ss")    time.format <- "%H:%M:%S"
  else if (tolower(time.format)=="hh:mmpm")     time.format <- "%I:%M%p"
  else if (tolower(time.format)=="hh:mm pm")    time.format <- "%I:%M %p"
  else if (tolower(time.format)=="hh:mm")       time.format <- "%H:%M"
  else {
     warning("'", time.format,"' is not ",
             "a supported time format. 'hh:mm' will be used ",
             "instead.\n", sep="")
     time.format <- "%H:%M"
  }

  # Determine the ideal spacing for the time

  # Number of seconds in the time range
  time.range <- as.numeric(difftime(time.vec[length(time.vec)], time.vec[1], units="s"))

  # If the range is longer than 24 hours
  if (time.range > 86400)
    warning("The time range is > 24 hours.  The resulting time axis labeling may not be appropriate.\n")

  # possible spacings
  possible.spacings <- c(1/60, 5/60, 10/60, 15/60, 30/60, 45/60,  # Seconds within a minute
                         1,2,5,10,20,30,45,  # Minutes within the hour
                         seq(60,600, by=30)) * 60  # 1 to 10 hours

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
  else if (ideal.spacing == (1*60))
    mod.factor <- 1*60

  # Find nearest even minute for 2 minute spacing
  else if (ideal.spacing == (2*60))
    mod.factor <- 2*60

  # Find the nearest 5 minute for 5, 10 minute spacings
  else if (ideal.spacing <= (10*60))
    mod.factor <- 5*60

  # Find the nearest 10 minute for 20 minute spacings
  else if (ideal.spacing == (20*60))
    mod.factor <- 10*60

  # Find the nearest 15 minute for 30, 45 minute spacings
  else if (ideal.spacing <= (45*60))
    mod.factor <- 15*60

  # Find the nearest 30 minute for all spacings 1 hour and larger
  else
    mod.factor <- 30*60

  # Calculate the starting point
  start <- min(time.vec) + as.numeric(as.logical(mod.factor)) * (mod.factor - as.numeric(min(time.vec)) %% mod.factor)

  # Make the time axis
  axis.POSIXct(side, at=seq(start, max(time.vec), by=ideal.spacing),
               format = time.format)

} # smartTimeAxis

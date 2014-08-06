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

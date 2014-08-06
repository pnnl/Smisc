# Function for timing an expression
timeIt <- function(expr,
                   units=c("automatic","seconds","minutes","hours","days"),
                   return.time=FALSE,
                   verbose=TRUE) {

  units <- match.arg(units)

  # Evalute the expression and time the result
  start <- proc.time()[3]
  out <- eval(expr)
  dur <- proc.time()[3] - start
  names(dur) <- NULL

  # Pick the best one:  > 2 min, use min.  > 2 hours, use hours, > 2 days, use days.
  if (units=="automatic") 
    units <- c("seconds","minutes","hours","days")[sum(dur >= c(0,120,7200,172800))]      

  div <- switch(units, seconds=1, minutes=60, hours=3600, days=86400)    

  if (verbose)
    cat(round(dur/div, 2), units, "\n")
  
  if (!return.time)
    invisible(out)
  else
    invisible(list(out=out, elapsed=dur/div, units=units))

} # timeIt

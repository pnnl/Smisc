##' Times the execution of an expression
##'
##' @details If \code{units = "automatic"}, then the units are choosen according to the
##' following rule: If the duration is < 2 min, use seconds.  Else if duration
##' < 2 hours, use minutes. Else if < 2 days, use hours.  Otherwise, use days.
##'
##' @export
##' @param expr Any R \code{\link{expression}}.
##'
##' @param units A character expression long enough to uniquely identify one of
##' "automatic", "seconds", "minutes", or "hours".  Defaults to "automatic".
##'
##' @param return.time \code{= TRUE} returns the elapsed time as one of the
##' elements in a list.  See "Value" below.
##'
##' @param verbose \code{= TRUE} prints the elapsed time in the requested units.
##'
##' @return If \code{return.time = FALSE}, invisibly returns the evaluation of
##' \code{expr}.  If \code{return.time = TRUE}, invisibly returns a list with
##' the following components:
##' \item{out}{The evaluation of \code{expr}}
##' \item{elapsed}{The elapsed time to evaluate \code{expr}}
##' \item{units}{The time units of the elapsed time}
##'
##' @author Landon Sego
##'
##' @keywords misc
##'
##' @seealso \code{\link{proc.time}}
##'
##' @examples
##'# We can assign the object within the call to timeIt():
##'timeIt(x1 <- rnorm(10^6))
##'str(x1)
##'
##'\donttest{
##'# We can just run the expression without assigning it to anything
##'timeIt(rnorm(10^7), units = "m")
##'
##'# Or we can assign the result externally
##'x2 <- timeIt(rnorm(10^7))
##'str(x2)
##'
##'# To store the elapsed time:
##'x3 <- timeIt(rnorm(10^7), verbose = FALSE, return.time = TRUE)
##'x3[c("elapsed","units")]
##'}

timeIt <- function(expr,
                   units = c("automatic","seconds","minutes","hours","days"),
                   return.time = FALSE,
                   verbose = TRUE) {

  units <- match.arg(units)

  # Evalute the expression and time the result
  start <- proc.time()[3]
  out <- eval(expr)
  dur <- proc.time()[3] - start
  names(dur) <- NULL

  # Pick the best one:  > 2 min, use min.  > 2 hours, use hours, > 2 days, use days.
  if (units == "automatic")
    units <- c("seconds","minutes","hours","days")[sum(dur >= c(0,120,7200,172800))]

  div <- switch(units, seconds = 1, minutes = 60, hours = 3600, days = 86400)

  if (verbose)
    cat(round(dur/div, 2), units, "\n")

  if (!return.time)
    invisible(out)
  else
    invisible(list(out = out, elapsed = dur/div, units = units))

} # timeIt

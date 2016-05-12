##' Calculates a sequence of CUSUM statistics
##'
##' Calculates a sequence of one-sided upper CUSUM statistics given the reference value and
##' the control limit.
##'
##' @details CUSUM is assumed to be of the form: \emph{C[i] = max(0, C[i-1] + X[i] - k)},
##' where the signal occurs when \emph{C[i] > h}.  Note that \code{X} can be the CUSUM scores, or weights,
##' given by the log-likelihood ratio, in which case \code{k = 0} would make sense.
##'
##' Plot method to come...
##' 
##' @export
##' @param X A numeric vector.
##'
##' @param k The reference value.
##'
##' @param h The upper control limit.
##'
##' @param initial The starting value of the CUSUM (\emph{C[0]}).
##'
##' @param reset Logical indicating whether the CUSUM is reset to  0 after crossing the control limit.
##'
##' @return A object of class 'cusum', which is a vector of the CUSUM statistics, along with the following attributes:
##' \code{X}, \code{k}, \code{h}, \code{initial}, and \code{reset} (which correspond to the original arguments provided to
##' the function) and \code{resetCounter}, a vector of integers corresponding to \code{cusum} that indicates when the 
##' CUSUM resets.
##'
##' @references Hawkins DM and Olwell DH. (1998) Cumulative Sum Charts and Charting for Quality Improvement. Springer.
##' 
##' @examples
##' y <- cusum(rnorm(50), 0.2, 2)
##' y
##'
##' # A look at the attributes
##' attributes(y)

# A Wrapper for the c method 'cusum'
# Calculates the CUSUM for a vector of data
# CUSUM of the form: C[i] = max(0, C[i-1] + X[i] - k)
# Signal occurs when C[i] > h

cusum <- function(X, k, h, initial = 0, reset = TRUE) {

  # Verify inputs
    
  stopifnotMsg(# X
               is.numeric(X) & (length(X) > 0),
               "'X' must be numeric and have positive length",
               
               # k
               if (is.numeric(k) & (length(k) == 1)) {
                 k >= 0  
               } else FALSE,
               "'k' must be a single, non-negative number",

               # h
               if (is.numeric(h) & (length(h) == 1)) {
                 h > 0
               } else FALSE,
               "'h' must be a single, positive number",

               # initial
               is.numeric(initial) & (length(initial) == 1),
               "'initial' must be a single number",

               # reset
               is.logical(reset) & (length(reset) == 1),
               "'reset' must be TRUE or FALSE")
               
  if (initial > h) {
    warning("Cusum initialized above the control limit")
  }

  # Calculate the cusum
  cusum <- .C("cusum",
              X = as.double(X),
              k = as.double(k),
              h = as.double(h),
              initial = as.double(initial),
              reset = as.integer(reset),
              upper = 1L, # upper set to TRUE
              n = as.integer(length(X)),
              cusum = double(length(X)),
#              stagger = double(length(X)),
              resetCounter = integer(length(X)))[c(1:5, 8, 9)]

  # Add in names if needed
  if (!is.null(names(X))) {
    names(cusum$cusum) <- names(cusum$resetCounter) <- names(cusum$X) <- names(X)
  }

  out <- cusum$cusum
  
  class(out) <- c("cusum", class(out))

  attributes(out) <- c(attributes(out),
                       cusum[c("X", "k", "h", "initial")],
                       list(reset = reset),
                       cusum["resetCounter"])

  # Return the list
  return(out)

} # cusum

##' @method print cusum
##'
##' @describeIn cusum Prints the \code{cusum} object by only showing the CUSUM statistics and suppressing the attributes.
##'
##' @param \dots Additional arguments to \code{\link{print.default}} or \code{\link{plot.default}}
##'
##' @export

print.cusum <- function(x, ...) {

  y <- x
  attributes(y) <- list(names = names(x))
  print(y, ...)

} # print.movAvg2


##' @method plot cusum
##'
##' @describeIn cusum Prints the \code{cusum} object by only showing the CUSUM statistics and suppressing the attributes.
##'
##' @param indexes A vector of indexes that select the elements of \code{X} that will be plotted
##'
##' @param overlayData A logical indicating whether the data are overlaid on the plot
##'
##' @export

plot.cusum <- function(x, indexes = NULL, overlayData = FALSE, ...) {

    
  y <- x
  attributes(y) <- list(names = names(x))
  print(y, ...)

} # print.movAvg2


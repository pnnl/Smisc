##' Calculates a sequence of Cusum statistics
##'
##' Calculates a sequence of one-sided upper Cusum statistics given the reference value and
##' the control limit.
##'
##' @details Cusum is assumed to be of the form: \emph{C[i] = max(0, C[i-1] + X[i] - k)},
##' where the signal occurs when \emph{C[i] > h}.  Note that \code{X} can be the Cusum scores, or weights,
##' given by the log-likelihood ratio, in which case \code{k = 0} would make sense.
##' 
##' @export
##' @param X A numeric vector.
##'
##' @param k The reference value.
##'
##' @param h The upper control limit.
##'
##' @param initial The starting value of the Cusum (\emph{C[0]}).
##'
##' @param reset Logical indicating whether the Cusum is reset to 0 after crossing the control limit.
##'
##' @param x Object of class \code{cusum}
##'
##' @param object Object of class \code{cusum}
##' 
##' @param \dots Additional arguments to \code{\link{print.default}} or \code{\link{plot.default}}.  Ignored by the \code{signal} method.
##'
##' @return A object of class \code{cusum}, which is a vector of the Cusum statistics, along with the following attributes:
##' \code{X}, \code{k}, \code{h}, \code{initial}, and \code{reset} (which correspond to the original arguments provided to
##' the function) and \code{resetCounter}, a vector of integers corresponding to \code{cusum} that indicates when the 
##' Cusum resets.
##'
##' @references Hawkins DM and Olwell DH. (1998) Cumulative Sum Charts and Charting for Quality Improvement. Springer.
##' 
##' @examples
##' y <- cusum(rnorm(50), 0.2, 2)
##' y
##'
##' # Plot the cusum
##' plot(y)
##'
##' # Show the indexes where the chart signaled
##' signal(y)
##' 
##' # A look at the attributes
##' attributes(y)

# A Wrapper for the c method 'cusum'
# Calculates the Cusum for a vector of data
# Cusum of the form: C[i] = max(0, C[i-1] + X[i] - k)
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
  cusum <- .C("cusum_c",
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
##' @describeIn cusum Prints the \code{cusum} object by only showing the Cusum statistics and suppressing the attributes.
##'
##' @export

print.cusum <- function(x, ...) {

  printWithoutAttributes(x, ...)

} # print.cusum


##' @method plot cusum
##'
##' @describeIn cusum Plots the \code{cusum} object.
##'
##' @param indexes A vector of indexes that select the elements of the cusum statistics that will be plotted.
##'
##' @param emphOOC A logical indicating whether out of control points should be emphasized in red.
##'
##' @export

plot.cusum <- function(x, indexes = NULL, emphOOC = TRUE, ...) {

  # Check inputs
  stopifnotMsg(if (!is.null(indexes)) {
                 if (is.numeric(indexes) & (length(indexes) <= length(x)) & (length(indexes) > 0)) {
                   all(indexes %in% 1:length(x))
                 } else FALSE
               } else TRUE,
               "'indexes' must be whole numbers in the set '1:length(x)', or NULL",
               is.logical(emphOOC) & (length(emphOOC) == 1),
               "'emphOOC' must be TRUE or FALSE")

  # If indexes are NULL, set to all possible to make selection easier
  if (is.null(indexes)) {
    indexes <- 1:length(x)
  }

  # Get the set of data points that will be plotted
  xvals <- indexes
  yvals <- x[indexes]

  # Get the control limit, as it is used often
  h <- attributes(x)$h
     
  # Set the default plot args
  defaultArgs <- list(x = indexes,
                      y = yvals,
                      xlab = "Index",
                      ylab = "Cusum Statistic",
                      ylim = range(yvals, h),
                      type = "b",
                      col = "Blue",
                      pch = 1,
                      cex = 1)

  # Blend in the supplied plot arguments from ...
  finalArgs <- blendArgs(defaultArgs, ...)
  
  # Create the plot if no reset
  if (!attributes(x)$reset) {
    do.call(plot, finalArgs)
  }
  else {

    # Make a blank plot and then add in the lines for each cusum run
    finalArgsReset <- finalArgs
    finalArgsReset$type <- "n"

    do.call(plot, finalArgsReset)

    # Add a connected line segment for each run
    cnt <- attributes(x)$resetCounter[indexes]

    for (i in unique(cnt)) {

      runInd <- cnt == i
 
      # Add data points first if they were requested
      do.call(lines, c(list(x = indexes[runInd], y = yvals[runInd]), finalArgs[c("type", "col", "pch", "cex")]))
        
    }

  }

  # Add in the control limit
  abline(h = h, col = "Red")

  # Color the signal points red
  if (emphOOC) {
    signalInd <- which(yvals > h)
    points(indexes[signalInd], yvals[signalInd], col = "Red", pch = finalArgs$pch, cex = finalArgs$cex)
  }

} # plot.cusum

##' @method signal cusum
##'
##' @describeIn cusum Prints the indexes in a \code{cusum} object that exceed the control limit
##'
##' @export

signal.cusum <- function(object, ...) {

  which(object > attributes(object)$h)
    
} # signal.cusum

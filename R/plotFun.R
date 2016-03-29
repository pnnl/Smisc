##'Plot one or more functions on a single plot
##'
##'A convenient wrapper function for plotting one or more functions on a single plot. If the function(s) is/are expensive to 
##'calculate, function values can be calculated in parallel.
##'
##' @export
##' @param fun A function or a list of functions to be plotted.  These functions should take a single, numeric vector argument and return
##' a corresponding vector of outputs.
##'
##' @param xlim A numeric vector with two elements that define the domain over which the function(s) will be evaluated
##' and plotted, just as in \code{\link{plot.default}} in the \pkg{graphics} package.
##'
##' @param col A vector of colors to use in the plotting. It's length should match the length of \code{fun}. See \code{\link{par}} for
##' more info about the \code{col} graphics parameter.
##'
##' @param lty A vector of line types to use in the plotting. It's length should match the length of \code{fun}. See \code{\link{par}} for
##' more info about the \code{lty} graphics parameter.
##'
##' @param type A single character indicating the type of plotting. This is passed to the \code{type} argument of
##' \code{\link{plot.default}}.
##'
##' @param legendLabels A character vector with descriptive names that will appear in the legend, corresponding to each function
##' If \code{NULL}, no legend is drawn. This character vector is passed to the \code{legend} argument in \code{\link{legend}}
##' from the \pkg{graphics} package.
##'
##' @param relX A numeric value in [0, 1] designating the relative horizontal (x) position of the legend in the plot.
##'
##' @param relY A numeric value in [0, 1] designating the relative vertical (y) position of the legend in the plot.
##'
##' @param nPoints The number of points that are evaluated and plotted for each function over the interval given by \code{xlim}.
##'
##' @param njobs The number of parallel jobs to spawn using \code{\link{doCallParallel}}.
##'
##' @param \dots Additional graphical arguments passed to \code{\link{plot.default}}, \code{\link{lines}}, and
##' \code{\link{legend}}.  If an argument name specified in \code{\dots} matches an argument name in any
##' of these three functions, the argument is passed to that function.
##' For example, the line width, \code{lwd}, would be passed to all three (\code{\link{plot.default}}, \code{\link{lines}}, and
##' \code{\link{legend}}).
##'
##' @return The plot of the function(s)
##'
##' @author Landon Sego
##'
##' @examples
##' # A single function with a single argument
##' f <- function(x) x^2
##' plotFun(f, c(-2, 3), col = "Black", lty = 2, las = 1)
##'
##' # A handful of beta density functions, note how they take a single argument
##' fList <- list(function(x) dbeta(x, 10, 10),
##'               function(y) dbeta(y, 3, 3),
##'               function(z) dbeta(z, 0.5, 0.50))
##'
##' # Plot them all on the same plot
##' plotFun(fList, c(0.0001, 0.9999), ylim = c(0, 3.5),
##'         col = c("Red", "Black", "Blue"), lty = rep(1, 3),
##'         xlab = "x", ylab = expression(f(x)),
##'         legendLabels = c("a = 10, b = 10", "a = 3, b = 3", "a = 0.5, b = 0.5"),
##'         relX = 0.6, relY = 1, lwd = 3, main = "Gamma Densities")

plotFun <- function(fun, xlim,
                    col = rainbow(length(fun)),
                    lty = 1:length(fun),
                    type = "l",
                    legendLabels = NULL,
                    relX = 0.7,
                    relY = 0.9,
                    nPoints = 1000,
                    njobs = 1, ...) {

  # Check arguments
  stopifnotMsg(# fun
               if (is.list(fun)) {
                 all(unlist(lapply(fun, is.function)))
               } else is.function(fun),
               "'fun' must be a function or a list of functions",

               # xlim
               if (is.numeric(xlim)) {
                 if (length(xlim) == 2) {
                   xlim[1] < xlim[2]
                 } else FALSE
               } else FALSE,
               "'xlim' must be a numeric 2-vector with the first element less than the second",

               # Length consistency
               length(fun) == length(col),
               "'length(col)' must equal 'length(fun)'",
               length(fun) == length(lty),
               "'length(lty)' must equal 'length(fun)'",               
               if (!is.null(legendLabels)) length(legendLabels) == length(fun) else TRUE,
               "'length(legendLabels)' must equal 'length(fun)'",

               # legendLabels
               if (!is.null(legendLabels)) is.character(legendLabels) else TRUE,
               "'legendLabels' must be a characer vector",

               # relX
               if (is.numeric(relX)) {
                 (length(relX) == 1) & (0 <= relX) & (relX <= 1)
               } else FALSE,
               "'relX' must be a single numeric value in [0, 1]",

               # relY
               if (is.numeric(relY)) {
                 (length(relY) == 1) & (0 <= relY) & (relY <= 1)
               } else FALSE,
               "'relY' must be a single numeric value in [0, 1]",

               # nPoints
               if (is.numeric(nPoints)) {
                 (length(nPoints) == 1) & (nPoints > 0) & (nPoints %% 1 == 0)
               } else FALSE,
               "'nPoints' must be a positive whole number",

               # njobs
               if (is.numeric(njobs)) {
                 (length(njobs) == 1) & (njobs > 0) & (njobs %% 1 == 0)
               } else FALSE,
               "'njobs' must be a positive whole number")
    
  # If fun is a single function, make it a list
  if (is.function(fun) & length(fun) == 1) {
    fun <- list(fun)
  }

  # Create a common sequence of x values
  xvec <- seq(xlim[1], xlim[2], length = nPoints)

  # Calculate the y values for each function
  yvals <- lapply(fun, function(fname) doCallParallel(fname, xvec, njobs = njobs, random.seed = rpois(1, 1000)))

  # Create the list with args for the plot.default command
  graphArgs <- list(...)

  # Add in ylims if they're not present
  if (!("ylim" %in% names(graphArgs))) {
    graphArgs$ylim <- range(unlist(lapply(yvals, range)))
  }

  # For prettier default axis labels
  x <- xvec
  y <- yvals[[1]]

  # Default optional args
  plotArgs1 <- list(x = quote(x), y = quote(y), type = type, xlim = xlim,
                    lty = lty[1], col = col[1])

  # Select arguments from graphArgs that could be used in plot.default()
  plotArgs2 <- if (as.logical(length(graphArgs))) {
                 graphArgs[names(graphArgs) %in%
                           setdiff(unique(c(names(formals(plot.default)), names(par()))), "...")]
               } else list()

  # Make the first plot
  do.call(plot, c(plotArgs1, plotArgs2))

  # If there are more functions to plot
  if (length(fun) > 1) {

    # Extract arguments that could be used for lines()
    lineArgs <- if (as.logical(length(graphArgs))) {
                  graphArgs[names(graphArgs) %in% names(par())]
                } else list()

    # Plot the additional functions
    for (i in 2:length(fun)) {
      do.call(lines, c(list(x = xvec, y = yvals[[i]], lty = lty[i], col = col[i]), lineArgs))
    }
  }

  # Make the legend
  if (!is.null(legendLabels)) {

    # Extract arguments that could be used for legend
    legendArgs <- if (as.logical(length(graphArgs)))
                    graphArgs[names(graphArgs) %in% names(formals(legend))]
                  else
                    list()

    # Make the legend
    do.call(legend, c(list(x = xlim[1] + relX * diff(xlim),
                           y = graphArgs$ylim[1] + relY * diff(graphArgs$ylim),
                           legend = legendLabels,
                           lty = lty,
                           col = col),
                      legendArgs))
  }

} # plotFun

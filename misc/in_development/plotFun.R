##'Plot one or more functions on a single plot
##'
##' @export
##' 
##' @param fun A function or a list of functions to be plotted.  These functions should take a single argument.
##'
##' @param xlim A numeric vector with two elements that define the domain over which the function(s) will be evaluated
##' and plotted, just as in \link{\code{plot.default}} in \pkg{graphics}.
##'
##' @param col A vector of colors to use in the plotting. It's length should match the length of \code{fun}.
##'
##' @param lty A vector of line types to use in the plotting. It's length should match the length of \code{fun}.
##'
##' @param legendLabels A text vector with descriptive labels of each function that will be provided in the legend.
##' If \code{NULL}, no legend is drawn. This text vector is passed to the \code{legend} argument in \code{\link{legend}}
##' from the \pkg{graphics} package.
##'
##' @param relX A numeric value in [0, 1] designating the relative horizontal (x) position of the legend on the plot.
##'
##' @param relY A numeric value in [0, 1] designating the relative vertical (y) position of the legend on the plot.
##'
##' @param nPoints The number of points that are evaluated and plotted for each function in \code{xlim}.
##'
##' @param \dots Additional graphical arguments passed to \code{\link{plot.default}}, \code{\link{lines}}, and
##' \code{\link{legend}}.
##'
##' @return The plot of the function(s)
##'
##' @author Landon Sego
##'
##' @examples
# A single function
f <- function(x) x^2
plotFun(f, c(-2, 3), col = "Black", lty = 2)

# A handful of beta density functions, note how they take a single argument
fList <- list(function(x) dbeta(x, 10, 10),
              function(y) dbeta(y, 3, 3),
              function(z) dbeta(z, 0.5, 0.50))

# Plot them all on the same plot
plotFun(fList, c(1e-10, 1 - 1e-10),
        col = c("Red", "Black", "Blue"), lty = rep(1, 3),
        xlab = "x", ylab = expression(f(x)),
        legendLabels = c("a = 10, b = 10", "a = 3, b = 3", "a = 0.5, b = 0.5"),
        relX = 0.8, relY = 0.5)


plotFun <- function(fun, xlim, col = rainbow(length(fun)), lty = 1:length(fun),
                    legendLabels = NULL, relX = 0.7, relY = 0.9, 
                    nPoints = 1000, ...) {
    
  # If fun is a single function, make it a list
  if (is.function(fun) & length(fun) == 1) 
    fun <- list(fun)
    
  # Check formats/conditions for arguments
  stopifnot(is.list(fun),
            all(unlist(lapply(fun, is.function))),
            is.numeric(xlim),
            length(xlim) == 2,
            xlim[1] < xlim[2],
            is.character(col),
            is.numeric(lty),
            length(fun) == length(col),
            length(fun) == length(lty),
            is.numeric(relX),
            length(relX) == 1,
            0 <= relX,
            relX <= 1,
            is.numeric(relY),
            length(relY) == 1,
            0 <= relY,
            relY <= 1,
            is.numeric(nPoints),
            nPoints > 0)

  # Create a common sequence of x values
  xvec <- seq(xlim[1], xlim[2], length = nPoints)

  # Calculate the y values for each function
  yvals <- lapply(fun, function(x) do.call(x, list(xvec)))

  # Determine the ylim
  ylimVals <- range(unlist(lapply(yvals, range)))

  # Create the list with args for the plot.default command
  graphArgs <- list(...)

  # Select arguments from graphArgs that could be used in plot.default()
  plotArgs2 <- ifelse1(as.logical(length(graphArgs)),
                       graphArgs[names(graphArgs) %in%
                                 setdiff(unique(c(names(formals(plot.default)),
                                                  names(par()))), "...")],
                       list())

  # Default optional args
  plotArgs1 <- list(x = xvec, y = yvals[[1]], type = "l", xlim = xlim,
                    ylim = ifelse1("ylim" %in% names(plotArgs2),
                                   plotArgs2$ylim, ylimVals),
                    lty = lty[1], col = col[1])

  browser()
  
  # Make the first plot
  do.call(plot, c(plotArgs1, plotArgs2))

  # If there are more functions to plot
  if (length(fun) > 1) {

    # Extract arguments that could be used for lines()
    lineArgs <- ifelse1(as.logical(length(graphArgs)),
                        graphArgs[names(graphArgs) %in% names(par())],
                        list())

    # Plot the addiional functions
    for (i in 2:length(fun))
      do.call(lines, c(list(x = xvec, y = yvals[[i]], lty = lty[i], col = col[i]), lineArgs))
  }

  # Make the legend
  if (!is.null(legendLabels)) {

    if (length(legendLabels) != length(fun))
      stop("Length of 'legendLabels' must match the length of 'fun'")

    # Extract arguments that could be used for legend
    legendArgs <- ifelse1(as.logical(length(graphArgs)),
                          graphArgs[names(graphArgs) %in% names(formals(legend))],
                          list())

    # Make the legend
    do.call(legend, c(list(x = xlim[1] + relX * diff(xlim),
                           y = ylimVals[1] + relY * diff(ylimVals),
                           legend = legendLabels,
                           lty = lty,
                           col = col),
                      legendArgs))

  }

} # plotFun

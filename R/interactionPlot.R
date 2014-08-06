# Landon's mods to interaction.plot.  Included the argument errorBar and jitterErrorBars.
# My additions are clearly bracketed

#  File src/library/stats/R/interaction.plot.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

##' Two-way Interaction Plot with Error Bar
##'
##' Plots the mean (or other summary) of the response for two-way combinations
##' of factors, thereby illustrating possible interactions.
##'
##' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##' This modification of \code{\link{interaction.plot}} adds the \code{errorBar} and
##' \code{jitterErrorBar} arguments.
##' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##'
##' By default the levels of \code{x.factor} are plotted on the x axis in their
##' given order, with extra space left at the right for the legend (if
##' specified). If \code{x.factor} is an ordered factor and the levels are
##' numeric, these numeric values are used for the x axis.
##'
##' The response and hence its summary can contain missing values. If so, the
##' missing values and the line segments joining them are omitted from the plot
##' (and this can be somewhat disconcerting).
##'
##' The graphics parameters \code{xlab}, \code{ylab}, \code{ylim}, \code{lty},
##' \code{col} and \code{pch} are given suitable defaults (and \code{xlim} and
##' \code{xaxs} are set and cannot be overridden).  The defaults are to cycle
##' through the line types, use the foreground colour, and to use the symbols
##' 1:9, 0, and the capital letters to plot the traces.
##'
##' @export
##'
##' @param x.factor a factor whose levels will form the x axis.
##' @param trace.factor another factor whose levels will form the traces.
##' @param response a numeric variable giving the response
##' @param fun the function to compute the summary. Should return a single real
##' value.
##' @param errorBar A list with required elements \code{height} and
##' \code{width}, and optional element \code{blankMiddle}. These correspond to
##' the arguments of \code{\link{vertErrorBar}}.  Defaults to \code{NULL}, in
##' which case no error bars are drawn
##' @param jitterErrorBar Logical indicating whether the x-values of the points
##' (and the error bars) should be jittered for easier readability. This
##' argument is ignored if \code{errorBar = NULL}.
##' @param type the type of plot (see \code{\link{plot.default}}): lines or
##' points or both.
##' @param legend logical. Should a legend be included?
##' @param trace.label overall label for the legend.
##' @param fixed logical.  Should the legend be in the order of the levels of
##' \code{trace.factor} or in the order of the traces at their right-hand ends?
##' @param xlab,ylab the x and y label of the plot each with a sensible
##' default.
##' @param ylim numeric of length 2 giving the y limits for the plot.
##' @param lty line type for the lines drawn, with sensible default.
##' @param col the color to be used for plotting.
##' @param pch a vector of plotting symbols or characters, with sensible
##' default.
##' @param xpd determines clipping behaviour for the \code{\link{legend}} used,
##' see \code{\link{par}(xpd)}.  Per default, the legend is \emph{not} clipped
##' at the figure border.
##' @param leg.bg,leg.bty arguments passed to \code{\link{legend}()}.
##' @param xtick logical. Should tick marks be used on the x axis?
##' @param xaxt,axes, graphics parameters to be passed to the plotting
##' routines.
##' @param list() graphics parameters to be passed to the plotting routines.
##' @note Some of the argument names and the precise behaviour are chosen for
##' S-compatibility.
##' @author Originates in the \code{stats} package in the R distribution,
##' modifications by Landon Sego
##' @seealso \code{\link{interaction.plot}}
##' @references Chambers, J. M., Freeny, A and Heiberger, R. M. (1992)
##' \emph{Analysis of variance; designed experiments.} Chapter 5 of
##' \emph{Statistical Models in S} eds J. M. Chambers and T. J. Hastie,
##' Wadsworth & Brooks/Cole.
##' @keywords hplot
##' @examples
##'
##' %%% BEGIN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##' require(graphics)
##'
##' with(ToothGrowth, {
##' interactionPlot(dose, supp, len, fixed=TRUE)
##' dose <- ordered(dose)
##' interactionPlot(dose, supp, len, fixed=TRUE, col = 2:3, leg.bty = "o")
##'
##' interactionPlot(dose, supp, len, fixed=TRUE, col = 2:3, type = "p",
##'                 errorBar = list(height = 3, width = 0.05, blankMiddle = 1),
##'                 jitterErrorBars = TRUE,
##'                 main = "Error bars have no meaning--just for illustration")
##' })
##' %%% END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##'
interactionPlot <-
    function(x.factor, trace.factor, response, fun = mean,

## BEGIN ##############################################################################

             errorBar = NULL,
             jitterErrorBars = FALSE,

## END ################################################################################

	     type = c("l", "p", "b", "o", "c"), legend = TRUE,
             trace.label=deparse(substitute(trace.factor)), fixed=FALSE,
             xlab = deparse(substitute(x.factor)), ylab = ylabel,
             ylim = range(cells, na.rm=TRUE),
             lty = nc:1, col = 1, pch = c(1L:9, 0, letters),
             xpd = NULL, leg.bg = par("bg"), leg.bty = "n",
             xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, ...)
{

## BEGIN ##############################################################################

    # Find out whether ylim has been changed by the user before it gets lazy-evaluated
    ylimChanged <- deparse(substitute(ylim)) != "range(cells, na.rm = TRUE)"


## END ################################################################################

    ylabel <- paste(deparse(substitute(fun)), "of ",
                    deparse(substitute(response)))
    type <- match.arg(type)
    cells <- tapply(response, list(x.factor, trace.factor), fun)
    nr <- nrow(cells); nc <- ncol(cells)
    xvals <- 1L:nr
    ## See if the x.factor labels are a sensible scale
    if(is.ordered(x.factor)) {
        wn <- getOption("warn")
        options(warn=-1)
        xnm <- as.numeric(levels(x.factor))
        options(warn=wn)
        if(!any(is.na(xnm))) xvals <- xnm
    }
    xlabs <- rownames(cells)
    ylabs <- colnames(cells)
    nch <- max(sapply(ylabs, nchar, type="width"))
    if(is.null(xlabs)) xlabs <- as.character(xvals)
    if(is.null(ylabs)) ylabs <- as.character(1L:nc)
    xlim <- range(xvals)
    xleg <- xlim[2L] + 0.05 * diff(xlim)
    xlim <- xlim + c(-0.2/nr, if(legend) 0.2 + 0.02*nch else 0.2/nr) * diff(xlim)
    dev.hold(); on.exit(dev.flush())

## BEGIN ##############################################################################

    if (!is.null(errorBar)) {

      # Check list
      stopifnot(is.list(errorBar))

      if (!all(names(errorBar) %in% c("height", "width", "blankMiddle")))
        stop("'errorBar' list must only have names 'height', 'width', and optionally, 'blankMiddle'")

      if (!all(c("height", "width") %in% names(errorBar)))
        stop("'errorBar' list must have names 'height', 'width', and optionally, 'blankMiddle'")

      # Change the ylim so error bars will not be chopped off if the user didn't change the defaults
      if (!ylimChanged)
        ylim <- ylim + c(- errorBar$height / 2, errorBar$height / 2)

      # Rearrange xvals a bit if jitter is requested
      if (jitterErrorBars) {

        xvals_orig <- xvals

        xvals <- matrix(NA, nrow = NROW(cells), ncol = NCOL(cells))

        for (i in 1:NCOL(xvals))
          xvals[,i] <- jitter(xvals_orig)

      }

    }

## END ################################################################################

    matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim,
            xlab = xlab, ylab = ylab, axes = axes, xaxt = "n",
            col = col, lty = lty, pch = pch)

## BEGIN ##############################################################################

    # Add in error bars
    if (!is.null(errorBar)) {

      if (!jitterErrorBars) {

        for (cn in 1:NCOL(cells)) {
          for (xv in 1:length(xvals))
            do.call("vertErrorBar", c(list(x = xvals[xv], center = cells[xv,cn]), errorBar))
        }

      }
      else {

        cn <- as.vector(cells)
        xv <- as.vector(xvals)

        for (i in 1:length(cn))
          do.call("vertErrorBar", c(list(x = xv[i], center = cn[i]), errorBar))

        # Restore xvals to it's original state
        xvals <- xvals_orig
      }

    }

## END ################################################################################

    if(axes && xaxt != "n") {
	## swallow ... arguments intended for matplot():
	axisInt <- function(x, main, sub, lwd, bg, log, asp, ...)
	    axis(1, x, ...)
	mgp. <- par("mgp") ; if(!xtick) mgp.[2L] <- 0
	axisInt(1, at = xvals, labels = xlabs, tick = xtick, mgp = mgp.,
		xaxt = xaxt, ...)
    }

    if(legend) {
        yrng <- diff(ylim)
        yleg <- ylim[2L] - 0.1 * yrng
        if(!is.null(xpd) || { xpd. <- par("xpd")
                              !is.na(xpd.) && !xpd. && (xpd <- TRUE)}) {
            op <- par(xpd = xpd)
            on.exit(par(op), add = TRUE)
        }
        text(xleg, ylim[2L] - 0.05 * yrng, paste("  ", trace.label), adj = 0)
        if(!fixed) {
            ## sort them on the value at the last level of x.factor
            ord <- sort.list(cells[nr,  ], decreasing = TRUE)
            ylabs <- ylabs[ord]
            lty <- lty[1 + (ord - 1) %% length(lty)]
            col <- col[1 + (ord - 1) %% length(col)]
            pch <- pch[ord]
        }

        legend(xleg, yleg, legend = ylabs, col = col,
               pch = if(type %in% c("p","b")) pch,# NULL works
               lty = if(type %in% c("l","b")) lty,# NULL works
               bty = leg.bty, bg = leg.bg)
    }

    invisible()
}

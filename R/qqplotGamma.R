qqplotGamma <- function(X, fit, title=NULL,
                        xlab="Theoretical quantiles of Gamma distribution",
                        ylab="Sample Quantiles", ...) {

   op <- par(las=1, pty="s")

   n <- length(X)

   # Calculate the sample and theoretical quantiles used to
   # draw the line.
   y1 <- quantile(X, c(0.25, 0.75))
   x1 <- qgamma(c(0.25, 0.75), shape=fit$shape, scale=fit$scale)
   slope <- diff(y1)/diff(x1)
   int <- y1[1] - slope * x1[1]

   # Generate the theoretical quantiles for plotting
   plot.x <- qgamma(ppoints(n), shape=fit$shape, scale=fit$scale)
   plot.y <- sort(X)

   # Calculate the goodness of fit "gof"
   SSE <- sum((plot.y - (int+slope*plot.x))^2)
   SST <- (length(plot.y)-1) * var(plot.y)
   gof <- (SST - SSE) / SST

   # For long series, points are revmoved from the middle of
   # the series to avoid bogging down the plot
   if (n > 300) {
      subset <- c(1:100,floor(seq(101,n-100,len=100)),(n-100):n)
      plot.x <- plot.x[subset]
      plot.y <- plot.y[subset]
   }

   title.2 <- "QQ Plot of Gamma distribution fit"
   if (is.null(title))
     title <- title.2
   else
     title <- paste(title, "\n", title.2)
   
   # Make the plot
   plot(plot.x, plot.y, col="Blue", font.main=1, xlab=xlab, ylab=ylab, main=title, ...)

   # Add in the line
   abline(int, slope)

   r.x <- diff(range(plot.x))
   r.y <- diff(range(plot.y))

   # Informational text
   text(min(plot.x)+0.05*r.x,max(plot.y)-0.1*r.y,paste("Fit =",round(gof,3)),pos=4)
   text(max(plot.x)-0.4*r.x,min(plot.y)+0.15*r.y,paste("shape =",round(fit$shape,3)),pos=4)
   text(max(plot.x)-0.4*r.x,min(plot.y)+0.1*r.y,paste("scale = ",round(fit$scale,3)),pos=4)


   par(op)
   
} # end qqplotGamma()


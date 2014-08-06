# 2-sided moving average which has a variety of shapes

movAvg2 <- function(y=NULL, bw=30, type=c("gaussian", "exponential", "linear", "uniform"),
                    furthest.weight = 0.01, center.weight = 1, plot=FALSE, ...) {

  type <- match.arg(type)

  if (type == "gaussian") {
    b <- (bw^2)/log(furthest.weight / center.weight)
    wts <- center.weight * exp((-bw:bw)^2 / b)
  }

  else if (type == "exponential") {
    b <- bw / log(furthest.weight / center.weight)
    wts <- center.weight * exp(abs(-bw:bw) / b)
  }

  else if (type == "linear") 
    wts <- (furthest.weight - center.weight) * abs(-bw:bw) / bw + center.weight

  else
    wts <- rep(1, 2 * bw + 1) 

  if (any(wts < 0))
    warning("Some of the window weights are < 0.\n")
    
  # Plot the unnormalized weights
  if (plot) {
    plot(-bw:bw, wts,
         xlim=c(-(bw+3),(bw+3)),
         ylim=c(0, max(wts)),
         lwd=2,
         col="Blue",
         type="b", xlab="Data Index",
         ylab="Window weights",
         main="Unormalized weights for moving window",
         font.main=1)
    abline(h=0)
  }

  # Calculate the moving average using smartFilter and normalized weights
  if (!is.null(y))
    return(smartFilter(y, wts / sum(wts), ...))
  
} # gaussMovAvg

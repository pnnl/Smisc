##' Calculate the highest posterior density credible interval for a unimodal density
##'
##' Calculate the highest posterior density credible interval for a unimodal density
##'
##' @export
##'
##' @rdname hpd
##' 
##' @param pdf Function that takes a single numeric vector argument that returns a vector
##' of probability density values
##'
##' @param support A numeric vector of length 2 giving the interval over which the random variable has
##' support (i.e. for which the pdf is positive).  For now, this must be a finite interval.
##' Intervals for random variables within infinite support
##' can still be calculated by setting the values of support to be suitably large and/or small.  See examples.
##' 
##' @param prob A numeric value in (0, 1] indicating the size of the desired probability for the credible
##' interval.
##'
##' @param cdf A function that takes a single (not necessarily vector) argument and returns the cumulative probability.
##' If \code{NULL}, the pdf is integrated as needed to calculate probabilities
##' as needed.  However, when possible, it's best to provide the cdf.
##'
## @param invcdf A function that takes a single argument in [0, 1] and returns the inverse of the cumulative distribution function.
## This is required if \code{support} is not finite.
##'
##' @return A list of class \code{hpd} that contains the following elements:
##' \begin{itemize}
##' \item{lower}{The lower endpoint of the highest posterior density interval}
##' \item{uper}{The lower endpoint of the highest posterior density interval}
##' \item{prob}{The acheived probability of the interval}
##' \item{cut}{The horizontal cut point that gave rise to the interval}
##' \item{mode}{The mode of the density}
##' \item{pdf}{The probability density function}
##' \item{support}{The support of the pdf}
##' \end{itemize}
##'
##' @author Landon Sego
##' 
##' @examples

hpd <- function(pdf, support, prob = 0.95, cdf = NULL) { #, invcdf = NULL) {

  # Basic checks
  stopifnot(is.function(pdf),
            is.numeric(support),
            is.vector(support),
            length(support) == 2,
            support[2] > support[1],
            if (!is.null(cdf)) is.function(cdf) else TRUE,
            is.numeric(prob),
            length(prob) == 1,
            prob > 0,
            prob <= 1)

  # Verify pdf is unimodal
  xseq <- seq(support[1], support[2], length = 1000)
  fdiff <- sign(diff(pdf(xseq)))
  ### TODO check this...
  
  
  # If the support is infinite on one or more of the bounds, find a finite values way out in the tail where the value is close to 0
  ## if (any(abs(support) == Inf)) {

  ##   if (is.null(invcdf))
  ##     stop("When 'support' is not finite, 'invcdf' must be provided")

  ##   # Replace the lower and upper values of the support with something large in magnitude but finite
    
  ##   # If the lower value is -Inf
  ##   if (support[1] == -Inf) 
  ##     support[1] <- invcdf(1e-12)

  ##   # If the upper value if Inf
  ##   if (support[2] == Inf) 
  ##     support[2] <- invcdf(1 - 1e-12)
  
  ## } # If the support is not finite
  
  # Find the mode
  peak <- optimize(pdf, lower = support[1], upper = support[2],
                   maximum = TRUE)$maximum

  # Attempt to calculate probability via integrating the pdf if cdf not provided
  if (is.null(cdf)) {

    pdfInterval <- function(lower, upper) {

      return(integrate(pdf, lower = max(lower, support[1]), upper = min(upper, support[2]))$value)
        
    } # pdfInterval

  } # If is.null(cdf)

  # For a given horizontal cut of the density, calculate the area under the curve
  area <- function(cut) {

      # Get the two endpoints in the support that correspond to the cut
      objFun <- function(x) {
        pvar(x, pdf(x), cut, sign(pdf(x) - cut))
         pdf(x) - cut
      }
      
      # To the left of the mode
      v1 <- uniroot(objFun, interval = c(support[1], peak - 1e-10), extendInt = "upX")$root
      pvar(v1, support[1], peak)

      # To the right of the mode
      v2 <- uniroot(objFun, interval = c(peak + 1e-10, support[2]), extendInt = "downX")$root
      pvar(v2, support[2], peak)
      
      # Find area under the curve
      if (is.null(cdf))
        achievedProb <- pdfInterval(v1, v2)
      else
        achievedProb <- cdf(v2) - cdf(v1)
      
      # Now the area under the curve
      return(list(lower = v1,
                  upper = v2,
                  prob = achievedProb))
                  
  } # area

  # Now solve for the cut that gives the desired probability
  cutSolution <- uniroot(function(y) {area(y)$prob - prob}, #; pvar(y, out); return(out)},
                         interval = c(1e-12, pdf(peak) - 1e-10))$root

  # Now return the interval and the mode
  out <- c(area(cutSolution), list(cut = cutSolution, mode = peak, pdf = pdf, support = support))
  class(out) <- c("hpd", class(out))

  return(out)
  
} # hpd

################################################################################
# Printing and plotting methods
################################################################################

##' @rdname hpd
##' @method print hpd
##' @S3method print hpd

print.hpd <- function(hpdObject) {
    
  return(hpdObject[c("lower", "upper", "prob")])
  
} # print.hpd

##' @rdname hpd
##' @method plot hpd
##' @S3method plot hpd
##' @params \dots Additional agruments that may be passed to \link{\code{plot.default}} or \link{\code{abline}}

plot.hpd <- function(hpdObject, ...) {

  ## TODO create default xlab and ylab values that can be overwritten
    
  if ("xlim" %in% names(list(...))) 
    plotFun(hpdObject$pdf, xlab = expression(x), ylab = expression(f(x)), ...)
  else 
    plotFun(hpdObject$pdf, hpdObject$support, xlab = expression(x), ylab = expression(f(x)), ...)      

  # Add in the lines
  abline(v = c(hpdObject$lower, hpdObject$upper), ...)
  abline(h = hpdObject$cut, col = "Gray")
  
} # plot.hpd


# A credible interval using the standard normal
## int <- hpd(dnorm, c(-5,5), prob = 0.90) 
## print(int)
## plot(int)

# The gamma density
int <- hpd(function(x) dgamma(x, shape = 2, rate = 0.5), c(0, 20),
           cdf = function(x) pgamma(x, shape = 2, rate = 0.5), prob = 0.8)
print(int)
plot(int)

# A credible interval using the Beta density
## dens <- function(x) dbeta(x, 7, 12)
## dist <- function(x) pbeta(x, 7, 12)
## int <- hpd(dens, c(0, 1), cdf = dist)
## print(int)
## plot(int)

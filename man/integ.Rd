\name{integ}
\alias{integ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simple numerical integration routine}
\description{
Estimates the integral of a real-valued function using Simpson's or the Trapezoid approximation
}
\usage{
integ(y, x = NULL, a = NULL, b = NULL, method = c("simpson", "trapezoid"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{Vector of f(x) values}
  
  \item{x}{Numeric vector of sorted x values, each element of \code{x}
    should have a corresponding element of {y}.  Only required for the
    trapezoid method.  Not required for the Simpson method.}

  \item{a}{The lower limit of integration, only required for the Simpson
    method.}
  
  \item{b}{The upper limit of integration, only required for the Simpson
    method.}
  
  \item{method}{The method of integration (can use just the first
    letter).  Defaults to \code{simpson}.}
}
\details{
  For the Simpson method, \code{y} is a numeric vector of f(x), evaluated at an odd number of evenly
  spaced x's in the interval [a,b].

  For the trapezoid method, the elements of \code{x} and \code{y} should
  correspond to one another, and \code{x} must be sorted in ascending
  order.  The lengths or \code{x} and \code{y} should be the same, and
  they may be odd or even. The elements of \code{x} may be irregularly spaced.
}
\value{
  A single numeric estimate of the integral of f(x) over [a, b]
  (Simpson) or over the range of \code{x} (trapezoid).
}
\references{
Ellis R, Gulick D. "Calculus: One and Several Variables,"
Harcourt Brace Jovanovich, Publishers: New York, 1991; 479-482.
}
  
\author{Landon Sego}
% \note{ ~~further notes~~
%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
  # The Beta density from 0 to 0.7
  integ(dbeta(seq(0, 0.7, length=401), 2, 5), a = 0, b = 0.7)

  # Checking result with the cdf
  pbeta(0.7, 2, 5)

  # f(x) = x^2 from 0 to 3
  integ(seq(0, 3, length=21)^2, a = 0, b = 3)

  # A quadratic function with both methods
  x <- seq(0, 3, length = 51)
  integ(x^2, x = x, method = "t")
  integ(x^2, a = 0, b = 3, method = "s")

  # Now a linear function
  x <- seq(0, 2, length = 3)
  y <- 2 * x + 3
  integ(y, x = x, method = "t")
  integ(y, a = 0, b = 2)
}
\keyword{math}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

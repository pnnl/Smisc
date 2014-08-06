\name{pcbinom}
\alias{pcbinom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A continuous version of the binomial cdf}
\description{
  Uses the incomplete beta function to calculate a continuous version of
  the binomial cumulative distribution function.
}
\usage{
pcbinom(x, n, p, lower.tail = TRUE, log.p = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Real valued vector of the number of successes.}
  \item{n}{Real valued vector, all elements in \code{[0, Inf)}, of the number of trials.}
  \item{p}{Real valued vector, all elements in \code{[0,1]}, of the probability of success.}
  \item{lower.tail}{logical; if \code{TRUE}, the probabilities are \code{P[X <= x]},
          otherwise, \code{P[X > x]}.}
  \item{log.p}{logical; if \code{TRUE}, probabilities \code{p} are returned as \code{log(p)}.}
}
\details{
\code{pcbinom} is equivalent to \code{\link{pbinom}} for integer values of
\code{n} and \code{x}.

  Note that \code{pcbinom} does not recycle vectors in the usual fashion.
  Each of the arguments \code{x}, \code{n}, and \code{p} should have the
  same length, or, one or more of them can have the same length, so long
  as the other arguments have length 1.  See examples below.

}
\value{
  Returns a continuous version of the binomial distribution function.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
\note{
  This function was based on \code{binom.c} in the R source code.
}
\seealso{\code{\link{pbinom}}, \code{\link{pbeta}}}
\examples{
x <- c(  2,   3,   5, 5.2,   5)
n <- c(  4,   5,   7,   7, 7.2)
p <- c(0.2, 0.1, 0.8, 0.8, 0.7)

pcbinom(x, n, p)
pbinom(x, n, p)

# These will work
pcbinom(c(7.3, 7.8), 12, 0.7)
pcbinom(c(7.3, 7.8), c(12,13), 0.7)
pcbinom(12.1, c(14.2,14.3), 0.6)

# But these won't
try(pcbinom(c(7.3, 7.8), c(12, 14, 16), 0.7))
try(pcbinom(c(7.3, 7.8), c(12, 14, 16), c(0.7, 0.8)))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{cheb.od}
\alias{cheb.od}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates the Chebysehev limit for a vector of data}
\description{
  Calculates the lower, upper, one-sided upper, and one-sided lower Chebyshev limits for
  a vector of data.
}
\usage{
cheb.od(the.data, unimodal = TRUE, rej.val = c(1/100, 1/1000, 1/10000))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{the.data}{A numeric vector of data}
  \item{unimodal}{\code{=TRUE} indicates the assumption that the data
    come from a unimodal distribution}
  \item{rej.val}{A vector of probability cutoff values}
}
\details{When \code{unimodal = TRUE}, the Vysochanskiï-Petunin
 inequality is used.  Otherwise, the traditional Chebyshev inequality is
 used.
}
\value{
  A matrix, one row per \code{rej.value}, with the following columns:
  \item{probabilities}{The probabilities from \code{rej.value}}
  \item{lower.cv}{The lower Chebyshev limit}
  \item{upper.cv}{The upper Chebyshev limit}
  \item{one.sided.upper.cv}{The one-sided upper Chebyshev limit}
  \item{one.sided.lower.cv}{The one-sided lower Chebyshev limit}
}
\references{
 For the Chebyshev inequality, see:
  
 http://en.wikipedia.org/wiki/Chebyshev\%27s_inequality
  
 The unimodal Chebyshev inequality is known as the Vysochanskiï-Petunin
 inequality.  See:
 
 http://en.wikipedia.org/wiki/Vysochanski\%C3\%AF-Petunin_inequality

 Also see:
 
 http://ieeexplore.ieee.org/iel5/10432/33126/01559688.pdf?arnumber=1559688

}
\author{Brett Amidan with additions from John Hathway and Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{cheb.2step.od}}}
\examples{

# Unimodal, normal example
cheb.od(rnorm(500), rej.val=10^(-2:-5))

# Bimodal example
cheb.od(c(rnorm(300), rnorm(300, 4, 1)), unimodal=FALSE)

}
\keyword{misc}


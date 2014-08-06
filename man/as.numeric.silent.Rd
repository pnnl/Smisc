\name{as.numeric.silent}
\alias{as.numeric.silent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Silent wrapper for coercing a vector to numeric}
\description{
Silent wrapper for coercing a vector to numeric
}
\usage{
as.numeric.silent(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of any type}
}
% \details{
% }
\value{
  If \code{as.numeric(x)} produces an error or warning,
  \code{x} is returned unchanged.  Otherwise, \code{as.numeric(x)} is returned.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{as.numeric}}}
\examples{
as.numeric.silent(c("this","that"))
as.numeric.silent(c("2893.9","9423.48"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{max.le.i}
\alias{max.le.i}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes the maximum of the vector up to the current index}
\description{
  For each index in a vector, computes the maximum of the vector from
  the beginning of the vector up to the current index
}
\usage{
max.le.i(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numeric or integer vector}
}
% \details{
% }
\value{
  In the sequence \code{x[1], x[2], ..., x[n]}, \code{max.le.i} returns the
  vector \code{y} such that for each \code{i = 1,...,n}, \code{y[i] = max(x[j]; j = 1,...,i)}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
max.le.i(1:10)
max.le.i(c(1,3,4,5,3,2,5,1,7,8,8,6))
max.le.i(c(1,3,4,5,3,2,5,1,7,8,8,6) + runif(12))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

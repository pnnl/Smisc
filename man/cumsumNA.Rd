\name{cumsumNA}
\alias{cumsumNA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes the cummulative sum of a vector without propogating NAs
}
\description{
Computes the cummulative sum of a vector without propogating NA's.  
}
\usage{
cumsumNA(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An integer or double vector
}
}
\details{
  If \code{x} is integer, then integer addition is used.  Otherwise,
  floating point (double) addition is used.
}
\value{
  The vector of cumulative sums.
  
  Elements in \code{x} that were \code{NA} will continue to be
  \code{NA}, but the \code{NA} will not be propogated.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Landon Sego
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cumsum}}
}
\examples{

# Compare to cumsum()
x <- as.integer(c(5, 2, 7, 9, 0, -1))
cumsum(x)
cumsumNA(x)

# Now with missing values
x[c(2,4)] <- NA
print(x)
cumsum(x)
cumsumNA(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{find.dframeEquiv}
\alias{find.dframeEquiv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Finds the level of numerical equivalence of two dataframes or matrices}
\description{
  Wrapper function for \code{dframeEquiv} that
  identifies the smallest \code{maxAbsError} and \code{maxRelError} for
  which two data frames or matrices are still equivalent
}
\usage{
find.dframeEquiv(d1, d2, minPrec = 5, maxPrec = 15, returnMatrix = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d1}{The first dataframe or matrix}
  \item{d2}{The dataframe or matrix that will be compared to \code{d1}}
  \item{minPrec}{An integer ranging from 0 to 22 indicating the smallest
    exponent in \code{maxAbsError} and \code{maxRelError} that will be considered}
  \item{maxPrec}{An integer ranging from 1 to 22 indicating the largest exponent
    in \code{maxAbsError} and \code{maxRelError} that will be
    considered.  It must be larger than \code{minPrec}.}
  \item{returnMatrix}{\code{=TRUE} returns a boolean matrix indicating the
  result of the comparison for the various combinations of
  \code{maxAbsError} and \code{maxRelError}}
}
\details{The function tests the equivalence of \code{d1} and
  \code{d2} over the matrix of combinations of \code{maxAbsError} and
  \code{maxRelError}, each with values \code{1e-minPrec, 1e-(minPrec+1),
    ..., 1e-(maxPrec-1), 1e-maxPrec}.

}
\value{
  If \code{d1} is not equal to \code{d2} when \code{maxAbsError=1e-minPrec} and
  \code{maxRelError=1e-minPrec} then \code{NULL} is returned, along with the
  message that describes the inequality. Otherwise, a list
  with the following components is returned:
  
  \item{leastAbsError}{A named vector indicating values of the
    the smallest \code{maxAbsError} and the corresponding
    \code{maxRelError} that resulted in equivalence}
  \item{leastRelError}{A named vector indicating values of the
    the smallest \code{maxRelError} and the corresponding
    \code{maxAbsError} that resulted in equivalence}
  \item{returnMatrix}{A boolean matrix whose rows (and rownames)
    correspond to the exponents of the \code{maxAbsError} and whose
    columns (and colnames) correspond to the exponents of the
    \code{maxRelError}.  Values of \code{TRUE} indicate combinations of
    \code{maxMaxError} and \code{maxRelError} for
    which the two dataframes/matrices were equivalent.}
}
%\references{}
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{dframeEquiv}}}
\examples{
# Number of rows different
a <- matrix(rnorm(20), nrow=4)
b <- a[1:3,]
find.dframeEquiv(a, b)

# Equivalent
x <- data.frame(x=letters[1:6], y=rnorm(6), z=rnorm(6))
y <- x
find.dframeEquiv(x, y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{padZero}
\alias{padZero}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pad a vector of numbers with zeros}
\description{
  Pad a numeric vector with zeros so that each element
  in the vector either has 1) the same number of characters or 2) the same
  number of trailing decimal places.
}
\usage{
padZero(vec, num = NULL, side=c("left","right"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{The numeric vector to be padded}
  \item{num}{The number of zeros that will be padded. If \code{NULL},
    the value is chosen based on the longest string in the vector.}
  \item{side}{The side to which the zeros are added.   }
}
\details{
    For \code{side='left'}, \code{num} is the number of characters that each element of the padded, output
    vector should have.  If \code{num=NULL}, the largest number of
    characters that appears in the vector is chosen for \code{num}.

    For
    \code{side='right'}, \code{num} is the number of decimal places to be
    displayed. If \code{num=NULL}, the number of decimals in the element with the largest number
    of decimal places is used.

    Note that \code{vec} must be numeric when
    \code{side='right'}.  However, \code{vec} may be character when \code{side='left'}.
}
\value{
  Character vector with the leading (or trailing) elements padded with zeros.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
  # Examples with 0's on the left
  padZero(c(1,10,100))
  padZero(c(1,10,100), num=4)

  # Examples with 0's on the right
  padZero(c(1.2, 1.34, 1.399), side="r")
  padZero(c(1.2, 1.34, 1.399), num=5, side="r")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{}% __ONLY ONE__ keyword per line

\name{timeStamp}
\alias{timeStamp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Imbeds the present datetime into a file name}
% \description{
% }
\usage{
timeStamp(description, extension)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{description}{Character string giving the base name of the file}
  \item{extension}{Character string giving the extension of the file
    (excluding the period)}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  Character string of the form \code{description_YYYY-MM-DD_HHMMSS.extension}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
timeStamp("aFilename", "txt")
}
\keyword{misc}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

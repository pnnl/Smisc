\name{factor2character}
\alias{factor2character}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts all factor variables in a dataframe to character variables}
% \description{
%   ~~ A concise (1-5 lines) description of what the function does. ~~
% }
\usage{
factor2character(dframe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dframe}{A dataframe}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  The same dataframe with the factor variables converted to character variables.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
x <- data.frame(a=factor(c(rep(1,4),rep(2,4),rep(3,4))), y=rnorm(12))
str(x)
x <- factor2character(x)
str(x)
}
\keyword{misc}% at least one, from doc/KEYWORDS
\keyword{manip}% __ONLY ONE__ keyword per line

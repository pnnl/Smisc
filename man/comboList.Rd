\name{comboList}
\alias{comboList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produces all possible combinations of a set of linear model predictors}
\description{
  Produces a list representing all possible combinations of linear model
  predictors
}
\usage{
comboList(n.pred, outFile = NULL, njobs = 1, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.pred}{
integer indicating the number of predictors
}
  \item{outFile}{
text string indicating the .Rdata file to which the returned list of
predictor combinations will be saved.  If NULL, then no file is saved.
}
  \item{njobs}{
Integer indicating the number of parallel jobs to be used in calculating
the combinations, using \code{\link{plapply}}
}
  \item{verbose}{
\code{= TRUE} indicates that a message regarding the saving of the output file will be printed
}
}
\details{
Uses \code{\link{combn}} to identify the combinations.
}
\value{
  A list of class \code{combolist} is invisibly returned with
  the two components shown below.  If
  \code{outFile} is not \code{NULL}, this same list is saved to \code{outFile}:
  \item{len}{The total number of combinations}
  \item{pList}{A list where each element contains an integer
    representation of one combination of the predictors}
}
%\references{
%}
\author{
Landon Sego
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fitAll}}
}
\examples{
x <- comboList(3)
print(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{df2list}
\alias{df2list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Row-wise conversion of a data frame to a list
}
\description{
Convert a data frame to a list, where each element of the output list
consists of a named list (or a named vector) containing a single row of the data frame.
}
\usage{
df2list(df, out.type = c("list", "data.frame", "vector"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{A data frame}
  \item{out.type}{Character string uniquely identifying 'list',
    'data.frame', or 'vector'.  If 'list', then each row of the data
    frame is output as a list.  If 'data.frame', then each row of the
    data frame is output as a 1-row data frame.
    If 'vector', then each row of the data frame is output as a named
    vector. However, for 'vector,' each column of the data set must be the same type.  Defaults to 'list'.}
}
%\details{
%}
\value{
A list where each element consists of a named list, single-row data
frame, or a named vector, containing a single row of the original data frame.
}
%\references{
%}
\author{
Landon Sego
}
%\note{
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{list2df}, \code{\link{as.list}}}
}
\examples{
d <- data.frame(a=1:3, b=letters[1:3])
df2list(d)

d1 <- data.frame(a=1:3, b=7:9)
df2list(d1, out.type = "v")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

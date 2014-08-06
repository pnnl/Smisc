\name{list2df}
\alias{list2df}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert a list to a data frame}
\description{
 Convert a list of vectors (or data frames) with same numbered lengths
 (or number of columns) into a data frame.
}
\usage{
list2df(vList, col.names = NULL, row.names = NULL,
        convert.numeric = TRUE, strings.as.factors = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vList}{List of vectors or data frames, each vector having the
    same length, \code{n}, or each data frame having the same number of
    columns, \code{n}.}
  \item{col.names}{Optional character vector of length \code{n} with column names
    that will be given to the output data frame.  If \code{col.names =
      NULL}, column names are extracted if possible from the column names (or names)
    of the data frames (or vectors).}
  \item{row.names}{Optional character vector with length equivalent to
    the length of \code{vList} containing the row names of the output
    data frame.  If \code{row.names = NULL}, row names from the data
    frames (or names of the \code{vList} elements) if possible.}
  \item{convert.numeric}{If \code{vList} is  list of vectors,
    \code{= TRUE} attempts to convert each column to
    numeric if possible using \code{\link{as.numeric.silent}}}
  \item{strings.as.factors}{If \code{vList} is a list of vectors,
    \code{= TRUE} converts character variables into factors using
    \code{\link{factor2character}}.}
 }
\details{It is permissible for \code{vList} to contain \code{NULL}
  elements. \code{list2df} performs numerous consistency checks to
  ensure that contents of \code{vList} which are combined into the
  resulting data frame
  are conformable, labeled consistently, of the equivalent class when
  necessary, etc.
}
\value{
  If \code{vList} is list of data frames, a data frame resulting from
  efficiently row binding
  the data frames in \code{vList} is returned.  If \code{vList} is a
  list of vectors,
  a data frame is returned where the first column contains the first elements
  of the list vectors, the second column contains the second elements of
  the list vectors, etc. 
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further note

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# For a list of data frames
z <- list(d1 = data.frame(a = 1:10, b = letters[1:10]),
          d2 = data.frame(a = 11:15, b = letters[11:15]))
list2df(z)

# For a list of vectors
x <- c("r1c1 1", "r2c1 2", "r3c1 3", "r4c4 4")
y <- strsplit(x, "\\ ")
list2df(y)
list2df(y, col.names=LETTERS[1:2])

z <- list(NULL, a = c(top = 10, bottom = 12), NULL, b = c(top = 15, bottom = 17))
list2df(z)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

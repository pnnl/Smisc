\name{sourceDir}
\alias{sourceDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{'sources' all files with '.R' or '.r' extensions in a directory}
% \description{
% }
\usage{
sourceDir(directory, recursive = FALSE, tryCatch = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{directory}{Character string indicating the path of the directory
  containing the R source files}
\item{recursive}{\code{=TRUE} descends into subdirectories of
  \code{directory}}
\item{tryCatch}{if \code{TRUE}, sourcing is protected in a try catch,
  i.e., if there is an error, \code{sourceDir} will continue to the next
  file.  If \code{FALSE}, \code{sourceDir} will stop if a file sources
  with an error.}
  \item{\dots}{Additional arguments to \code{source} function}
}
% \details{
% }
\value{
  Invisibly returns a character vector containing
  the files that were identified for sourcing.

  Also prints a message indicating whether each file was sourced
  correctly or not.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# Source R files in the current directory
sourceDir(".")

# Source R files in the directory "c:\Work\myRcode"
# sourceDir("c:/Work/myRcode")
}
\keyword{misc}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

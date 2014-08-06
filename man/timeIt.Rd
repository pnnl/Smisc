\name{timeIt}
\alias{timeIt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Times the execution of an expression}
\description{
  Times the execution of an expression.
}
\usage{
timeIt(expr, units = c("automatic", "seconds", "minutes", "hours"),
       return.time=FALSE,  verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{Any R expression}
  \item{units}{A character expression long enough to uniquely identify
    one of "automatic", "seconds", "minutes", or "hours".  Defaults to
    "automatic".}
  \item{return.time}{\code{=TRUE} returns the elapsed time as one of the
    elements in a list.  See "Value" below.}
  \item{verbose}{\code{=TRUE} prints the elapsed time in the requested units}
}
\details{If \code{units="automatic"}, then the units are choosen according
  to the following rule:  If the duration is < 2 min, use seconds.  Else
  if duration < 2 hours, use minutes. Else if < 2 days, use hours.
  Otherwise, use days.
}
\value{
  If \code{return.time = FALSE}, invisibly returns the evaluation of \code{expr}.
  If \code{return.time = TRUE}, invisibly returns a list
  with the following components:

  \item{out}{The evaluation of \code{expr}}
  \item{elapsed}{The elapsed time to evaluate \code{expr}}
  \item{units}{The time units of the elapsed time}

}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
  # We can assign the object within the call to timeIt():
  timeIt(x1 <- rnorm(10^7))
  str(x1)

  # We can just run the expression without assigning it to anything
  timeIt(rnorm(10^7), units="m")

  # Or we can assign the result externally
  x2 <- timeIt(rnorm(10^7))
  str(x2)

  # To store the elapsed time:
  x3 <- timeIt(rnorm(10^7), verbose=FALSE, return.time=TRUE)
  x3[c("elapsed","units")]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

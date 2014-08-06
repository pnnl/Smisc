\name{umvue.ln}
\alias{umvue.ln}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes UMVUEs of lognormal data}
\description{
  Computes UMVU estimates of the mean, the standard error of the mean,
  and the standard deviation of lognormally distributed data.
}
\usage{
umvue.ln(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector of lognormal data}
}
\details{
  Calculates equations 13.3, 13.5, and 13.6 of Gilbert (1987).
}
\value{
  Returns a named vector with the following components
  \item{mu}{The UMVUE of the mean}
  \item{se.mu}{The UMVUE standard error of the mean}
  \item{sigma}{The UMVUE of the standard deviation}
}
\references{Gilbert, Richard O. (1987) Statistical Methods for
  Environmental Pollution Monitoring, John Wiley & Sons, Inc. New York,
pp 164-167.}
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

# Test from Gilbert 1987, Example 13.1, p 166
x <- c(3.161, 4.151, 3.756, 2.202, 1.535, 20.76, 8.42, 7.81, 2.72, 4.43)
y <- umvue.ln(x)
print(y)

# Results from PRO-UCL 4.00.02:

# MVU Estimate of Mean			  5.6544289
# MVU Estimate of Standard Error of Mean  1.3944504
# MVU Estimate of SD			  4.4486438

# Compare with Gilbert's results (which have rounding error)
mu.1.hat <- round(y[1], 2)
s2.mu.1.hat <- round(y[2]^2, 2)
s2.1.hat <- round(y[3]^2, 1)
pvar(mu.1.hat, s2.mu.1.hat, s2.1.hat)

}
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

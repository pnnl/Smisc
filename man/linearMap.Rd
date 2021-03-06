% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearMap.R
\name{linearMap}
\alias{linearMap}
\title{Linear mapping of a numeric vector or scalar}
\usage{
linearMap(x, D = range(x), R = c(0, 1))
}
\arguments{
\item{x}{a numeric vector}

\item{D}{a vector with 2 elements, the first being the lower endpoint of
the domain, the upper being the upper endpoint of the domain. Note
\code{R[1]} must be less than \code{R[2]}.}

\item{R}{a vector with 2 elements indicating the range of the linear
mapping. \code{R[1]} is mapped to \code{D[1]}, and \code{R[2]} is mapped to
\code{D[2]}.}
}
\value{
The linear mapping of \code{x} from \code{D} to \code{R}
}
\description{
Linear mapping of a numeric vector or scalar from one closed interval
to another
}
\details{
The mapping is \eqn{f : D --> R}, where \eqn{f(D[1]) = R[1]} and
\eqn{f(D[2]) = R[2]}.
}
\examples{
x <- seq(0, 1, length = 5)

# An increasing linear map
linearMap(x, R = c(4, 7))

# A decreasing map
linearMap(x, R = c(7, 4))

# A shift
linearMap(x, R = c(-1, 0))

# The identity map:
y <- linearMap(x, D = c(0, 1), R = c(0, 1))
identical(y, x)
}
\author{
Landon Sego
}

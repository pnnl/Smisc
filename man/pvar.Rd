\name{pvar}
\alias{pvar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prints the name and value of one or more objects}
\description{
  A convenience function for writing the names and values of objects to the
  session window (and/or to another object).
  Especially useful to keep track of variables within loops.
}
\usage{
pvar(..., digits = NULL, abbrev = NULL, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Objects whose names and values are to be printed,
    separated by commas. Can also be a simple list.}
  \item{digits}{Number of digits to display for numeric objects.  Defaults
    to \code{NULL}, which corresponds to no restriction on the number of digits}
  \item{abbrev}{Number of characters to display for character objects.
    Defaults to \code{NULL}, which corresonds to no restriction on the
    number of characters.}
  \item{verbose}{\code{=TRUE} writes the value of the object(s) to the session window}
}
\details{Input objects can be numeric, character, and/or logical.  They can
  also be atomic or vectors.  It will accept data frames and matrices
  without error, but the results won't be easily readable.  
}
\value{
  Invisibly returns a character string containing the names of the
  objects and their values
}
% \references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
x <- 10
y <- 20.728923
z <- "This.long.string"

pvar(x,y,z)
pvar(x,y,z, digits=2)
pvar(x,y,z, abbrev=4)
pvar(x,y,z, digits=2, abbrev=4)

# values can be vectors too
x <- 10:12
y <- c("This","That")
v2 <- pvar(x, y, verbose=FALSE)
v2

# Or a simple list
pvar(list(x = 1:2, y = "this", z = TRUE))

# Can be useful for keeping track of iterations in loops
for (i in 1:3) {
  for (j in letters[1:2]) {
    for (k in c("this","that")) {
      pvar(i, j, k)
    }
  }
}

}
\keyword{misc}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

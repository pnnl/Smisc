\name{load.object}
\alias{load.object}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Loads and returns the first object in an ".Rdata" file}
\description{
Loads and returns the first object in the ".Rdata" file.  This is useful
for naming the object in the ".Rdata" something other than the name it
was saved as. 
}
\usage{
load.object(Rdata.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Rdata.file}{A character string containing the '.Rdata' filename}
}
\details{
If the '.Rdata' file contains more than one object, only the first
object is returned and a warning is produced.
}
\value{
  The first object in the '.Rdata' file.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
% \seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# Make an example object
x <- data.frame(a=1:10, b=rnorm(10))

# Create a unique filename with a time stamp
# imbedded in the filename
file.name <- timeStamp("demo_load_object", "Rdata")

# Save and reload the file
save(x, file=file.name)
y <- load.object(file.name)
print(y)

# Delete the saved object
unlink(file.name)

}
\keyword{misc}% at least one, from doc/KEYWORDS
%\keyword{manip}% __ONLY ONE__ keyword per line

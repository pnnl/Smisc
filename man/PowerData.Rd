\name{PowerData}
\Rdversion{1.1}
\alias{PowerData}
\docType{data}
\title{
An example of power data
}
\description{
A small, altered, subset of total rack power for one of the racks in a
data center, to be used for illustration.
}
\usage{data(PowerData)}
\format{
  The format is:
 Named num [1:12] 5.15 5.15 5.15 5.16 5.16 ...
 - attr(*, "names")= chr [1:12] "2008-05-06 17:00:01" "2008-05-06 17:00:17" "2008-05-06 17:00:21" "2008-05-06 17:00:32" ...
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(PowerData)
print(PowerData)
rm(PowerData, envir=.GlobalEnv)
}
\keyword{datasets}

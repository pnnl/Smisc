\name{killSlurm}
\alias{killSlurm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Kill a SLURM job from R
}
\description{
  Kill a SLURM job from R using \code{scancel job.id} 
}
\usage{
killSlurm(job.id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{job.id}{
SLRUM job number.  May be passed as a numeric value or as a string.
}
}
\details{
If the job is not present in the queue, then \code{scancel} command is
not issued.  This function is used for error control in \code{\link{piclapply}}.
}
\value{
Nothing.  Only a message indicating whether or not the job was canceled.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Landon Sego
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{piclapply}}.  
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

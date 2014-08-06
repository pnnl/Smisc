\name{checkBatch}
\alias{checkBatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine the status of an R batch job running in the background
}
\description{
Determine the status of an R batch job running in the background. Useful
if other processes need to wait on the completion of another job.
}
\usage{
checkBatch(file, check.interval.sec = 2, max.hours = 24, wait = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The filename of a .Rout file from an R batch job.
}
  \item{check.interval.sec}{
The number of seconds to wait between
    checking to see whether the job has completed.
}
  \item{max.hours}{
The maximum number of hours to wait for the job to complete.
}
  \item{wait}{
\code{=TRUE} indicates that \code{checkBatch} will wait until the
job complete.  If \code{FALSE}, it does not wait and simply returns the
status of the job.
}
}
\details{
If \code{proc.time()} is found in the .Rout file, the job is considered
to have completed successfully.  If \code{Execution halted} is found,
the job is considered to have failed.  If neither is found, the job is
considered to be still running.
}
\value{
  A text string with one of 3 values:
  \item{success}{If the job has completed successfully}
  \item{fail}{If the job has failed}
  \item{incomplete}{If the job is still running}
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# This example designed for Unix
# Make a quick batch file and launch it
cat("mean(rnorm(5*10^7))", file="test_checkBatch_tmp.R")

# Run this manually...
system("R CMD BATCH test_checkBatch_tmp.R", wait=FALSE)

# Wait briefly for the job to launch
Sys.sleep(0.5)

# This one, if lauched immediately after the system call above, shoul
# return "incomplete"
checkBatch("test_checkBatch_tmp.Rout", wait = FALSE)

# This one should wait until it finishes and return "success"
checkBatch("test_checkBatch_tmp.Rout", check.interval.sec = 1, max.hours = 0.1)

# Delete temporary files
unlink(c("test_checkBatch_tmp.R", "test_checkBatch_tmp.Rout"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}

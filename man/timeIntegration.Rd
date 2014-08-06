\name{timeIntegration}
\Rdversion{1.1}
\alias{timeIntegration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Approximate the integral of a vector of data over time
}
\description{
Integrate a series over time by calculating the area under the "curve"
of the linear interpolation of the series. 
}
\usage{
timeIntegration(data, time=names(data), lower=time[1], upper=time[length(time)],
                check.plot = FALSE, units=c("hours", "minutes", "seconds"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Vector of numerical data    
}
\item{time}{
Vector of timestamps which correspond to \code{data}. These can either character or POSIXct.  
}
  \item{lower}{
The time (character or POSIXct) of the lower bound of the integration
}
\item{upper}{
The time (character or POSIXct) of the upper bound of the integration  
}
  \item{check.plot}{
\code{=TRUE} makes a plot which illustrate the integration.
}
  \item{units}{
    The units of integration, defaults to hours.  It is only required to supply enough characters to uniquely complete the name.
  }
}
\details{
If \code{upper} or \code{lower} does not
correspond to a data point, a linear interpolation is made between the
two neighboring time points to predict the resulting data value.
}
\value{
The approximation of the integral by joining the points in the series in a linear fashion and calculating the area under this "curve".
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

% \seealso{
% }
\examples{

 # Some example power data
 data(PowerData)

 # For some reason, making the graph in Unix/Linux takes forever...so I write it to a pdf

 # Open the pdf if using Unix
 unix <- .Platform$OS.type == "unix"

 # Save this to a file
 if (unix) {
  fOut <- openDevice("tmpTimeInt.pdf", height=7, width=14)
 } else {
   X11(height=7, width=14)
 }

 par(mfrow=c(1,2))

 # Calculate the kilowatt-minutes, display graph which shows how the
 # integration is done
 int1 <- timeIntegration(PowerData,
                        # Convert to POSIXct in order to subtract time
                        lower = "5/6/2008 17:00:09",
                        upper = "5/6/2008 17:01:36",
                        check.plot=TRUE,
                        units = "m")

 int2 <- timeIntegration(PowerData, check.plot=TRUE, units="m")
 
 # close the graphics device
 if (unix) {
   dev.off()
   system(paste("acroread", fOut), wait=FALSE)  
 }

 # Print the outcome
 pvar(int1, int2)


 # Some other tests:
 # This should be 2
 timeIntegration(c(1,3), c("07-May-2009 15:00", "07-May-2009 16:00"))
 # This should be 1
 timeIntegration(c(1,3), c("07-May-2009 15:30", "07-May-2009 16:00"))
 # This should be 4
 timeIntegration(c(1,3), c("07-May-2009 15:30", "07-May-2009 17:30"))

 # clean up
 z <- readline("Hit Enter when finished to close the example >>")
 if (unix)
   unlink(fOut)
 rm(PowerData, envir=.GlobalEnv)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

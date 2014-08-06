\name{sort.data.frame}
\alias{sort.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sorts a dataframe by one or more variables}
\description{
  Sorts a dataframe by one or more variables, ascending or descending.
}
\usage{
sort.data.frame(form, dat)
sort.data.frame(dat, form)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{A formula with the variable names to use for sorting}
  \item{dat}{The dataframe to sort}
}
\details{The order of the arguments \code{form} and \code{dat} is
  interchangeable.  In the formula, use + for ascending, - for
  decending. Sorting is left to right in the formula. 
  For example, to sort the dataframe \code{Oats} with sorting variables \code{Block} and
  \code{Variety}, useage is either of the following: 

  \code{sort.data.frame(~ Block - Variety, Oats)}  or
  
  \code{sort.data.frame(Oats, ~ - Variety + Block)} 

}
\value{
   The sorted dataframe
}
% \references{}  
\author{Kevin Wright with ideas from Any Liaw and small edits by Landon Sego}
%\note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
d = data.frame(b=factor(c("Hi","Med","Hi","Low"),levels=c("Low","Med","Hi"), 
               ordered=TRUE),
               x=c("A","D","A","C"),y=c(8,3,9,9),z=c(1,1,1,2))

# Sort by descending z, descending b
sort.data.frame(~-z-b,d)

# Sort by ascending x, ascending y, and ascending z
sort.data.frame(~x+y+z,d)

# Sort by descending x, ascending y, ascending z
sort.data.frame(~-x+y+z,d)

# Sort by ascending x, descending y, ascending z
sort.data.frame(d,~x-y+z) 
}
\keyword{manip}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{valid.email.address}
\alias{valid.email.address}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Indicates whether a vector of email addreses may be valid}
\description{
Indicates whether a vector of email addreses may be valid
}
\usage{
valid.email.address(vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{Character vector of email addresses}
}
\details{
  Checks the following conditions:
  
  1: Only 1 @ allowed
  
  2: Should have text on both sides of the @
  
  3: Text on right hand side (RHS) of @ should have at least one dot
  
  4: RHS text should not end with a dot
  
  5: The suffix of the RHS should have 2 or 3 characters (e.g. 'com', 'net', 'uk')

  6: RHS should not begin with a dot
  
  7: No illegal characters--however, it does't check for a backslash
  
}
\value{
  Logical vector which is \code{TRUE} when the email address is valid.
}
\references{Based in part on \url{http://groups.google.com/group/eaut/web/rules-for-valid-email-addresses}}
\author{Landon Sego}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
 valid.email.address(c(NA, "@","this@that","this@that@that","this@that.com.ar",
                       "@this.com","that@","this@that.","this@.that","this@.that.com",
                        "this@that.com","this(@that.com","this@that .com"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

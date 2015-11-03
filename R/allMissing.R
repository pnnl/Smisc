##' Identifies missing rows or columns in a data frame or matrix
##'
##' Indicates which rows or columns in a data frame or matrix are completely
##' missing (all values are NA's).
##'
##' @export
##' @param dframe A data frame or a matrix
##'
##' @param byRow \code{= TRUE} will identify rows that have all missing values.
##' \code{= FALSE} identifies entire missing columns
##'
##' @return A logical vector that is true if all the elements in the
##' corresponding row (or column) are NA's.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{complete.cases}}
##'
##' @keywords misc
##'
##' @examples
##'
##' # Start off with a simple data frame that has a few missing values
##' d1 <- data.frame(x=c(3,4,NA,1,10,NA), y=c(NA,"b","c","d","e",NA))
##' d1
##'
##' # Identify rows were the entire row is missing
##' allMissing(d1)
##'
##' # Only removes rows where all the values are missing
##' d1[!allMissing(d1),]
##'
##' # All missing can also be used to identify if any of the
##' # columns are 'all missing'
##' d2 <- data.frame(x=c(rnorm(3), NA, rnorm(6)), y=rep(NA,10), z=letters[1:10])
##' d2
##'
##' # Look for columns that are all missing
##' allMissing(d2, byRow=FALSE)
##'
##' # Remove columns where all the values are missing
##' d2[,!allMissing(d2, byRow = FALSE)]
##'
allMissing <- function(dframe, byRow = TRUE) {

  if ((!is.matrix(dframe)) & (!is.data.frame(dframe)))
    stop("'", deparse(substitute(dframe)),
         "' must be a matrix or a data frame.\n")

  if (byRow)
    margin <- 1
  else
    margin <- 2

  f <- function(x) all(is.na(x))

  return(apply(dframe, margin, f))

} # end allMissing()

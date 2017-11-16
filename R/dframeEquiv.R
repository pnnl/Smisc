##' Examines the equivalence of two dataframes or matrices
##'
##' Checks whether two data objects (data frames and/or matrices) are
##' equivalent and returns a descriptive message describing the result.
##'
##' \code{d1} and \code{d2} do not both have to be of the same mode; i.e.
##' \code{d1} could be a dataframe and \code{d2} could be a matrix.  If the
##' number of rows or the number of columns differ, then no further comparisons
##' are made.  If the colnames or rownames differ, then those differences are
##' noted and comparison continues.  If two corresponding elements are both
##' \code{NA}, then they are considered equivalent.  Likewise, \code{Inf} is
##' considered equivalent to \code{Inf} and \code{-Inf} is considered
##' equivalent to \code{-Inf}.  Factors in dataframes are converted to
##' character strings prior to comparison.  Comparisons are made one column at
##' a time.
##'
##' If a particular column from both objects are numeric, then for two
##' corresponding values, say, \code{a} and \code{b}, equivalence is declared
##' if one or more of the following occurs: 1) \code{a == b}, 2) \code{abs(a -
##' b) < maxAbsError}, 3) \code{abs((a - b) / b) < maxRelError} if \code{abs(b)
##' > abs(a)}, or \code{abs((a - b) / a) < maxRelError} if \code{abs(b) >=
##' abs(a)}.
##'
##' If both columns are not numeric, they are coerced (if need be) to character
##' and then compared directly.
##'
##' @export
##' @param d1 The first dataframe or matrix
##'
##' @param d2 The dataframe or matrix that will be compared to \code{d1}
##'
##' @param maxAbsError Numeric values whose absolute difference is less than
##' \code{maxAbsError} will be declared equivalent
##'
##' @param maxRelError Numeric values whose relative difference is within
##' \code{maxRelError} will be declared equivalent
##'
##' @param verbose \code{=TRUE} prints the result of the comparison
##'
##' @return Invisibly returns a list with the following components.  (If the
##' matrices do not have the same dimensions or the same colnames and rownames,
##' then \code{frac.equiv}, \code{loc.equiv}, and \code{equiv.matrix} are all
##' \code{NULL}).  \item{equiv}{\code{=TRUE} if \code{d1} is equivalent to
##' \code{d2}} \item{msg}{Messages that describe the comparison.  (These are
##' printed when \code{verbose=TRUE}.)} \item{frac.equiv}{The fraction of
##' matrix elements that are equivalent} \item{loc.inequiv}{A data frame
##' indicating the row and column coordinate locations of the elements that are
##' not equivalent} \item{eqiv.matrix}{A boolean matrix with the same dimension
##' as \code{d1} and \code{d2}, indicating the equivalent elements}
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{all.equal}}, \code{\link{identical}}
##'
##' @references
##' \url{https://randomascii.wordpress.com/category/floating-point}
##' 
##' @keywords misc
##' 
##' @examples
##'
##' # Number of rows different
##' dframeEquiv(matrix(rnorm(20), nrow = 4),
##'             matrix(rnorm(25), nrow = 5))
##'
##' # Number of columns different
##' dframeEquiv(matrix(rnorm(16), nrow = 4),
##'             matrix(rnorm(20), nrow = 4))
##'
##' # Rownames differ
##' dframeEquiv(matrix(rnorm(9), nrow = 3, dimnames = list(1:3, NULL)),
##'             matrix(rnorm(9), nrow = 3, dimnames = list(letters[1:3], NULL)))
##'
##' # Colnames differ
##' dframeEquiv(matrix(rnorm(9), nrow = 3, dimnames = list(NULL, 1:3)),
##'             matrix(rnorm(9), nrow = 3, dimnames = list(NULL, letters[1:3])))
##'
##' # Not equivalent
##' x <- data.frame(x = factor(c(1,1,2,2,3,3)), y = rnorm(6))
##' y <- data.frame(x = factor(c(1,2,2,2,3,3)), y = c(x$y[-6],rnorm(1)))
##' dframeEquiv(x, y)
##'
##' # Look at discrepancies
##' out <- dframeEquiv(x, y)
##' out
##'
##' # Equivalent
##' x <- data.frame(x = letters[1:6], y = 0:5)
##' y <- x
##' dframeEquiv(x, y)
##'
dframeEquiv <- function(d1, d2, maxAbsError = 1e-12, maxRelError = 1e-14, verbose = TRUE) {

  d1.d2 <- paste("'", deparse(substitute(d1)), "' and '",
                 deparse(substitute(d2)), "'", sep="")

  # Initialize outputs
  frac.equiv <- loc.inequiv <- equiv.mat <- msg <- NULL

  if (!((is.data.frame(d1) | is.matrix(d1)) &
        (is.data.frame(d2) | is.matrix(d2))))
    stop(d1.d2, " must both be data frames or matrices.")

  keep.checking <- TRUE

  # Check for number of rows
  if (NROW(d1) != NROW(d2)) {
    msg <- paste(d1.d2, "have a different number of rows.")
    keep.checking <- FALSE
  }

  # Check for number of columns
  if (NCOL(d1) != NCOL(d2)) {
    msg <- c(msg, paste(d1.d2, "have a different number of columns."))
    keep.checking <- FALSE
  }

  if (keep.checking) {

    # Check colnames
    colStatus <- sum(c(is.null(colnames(d1)), is.null(colnames(d2))))
    if ((colStatus == 1) | ((colStatus == 0) & (any(colnames(d1) != colnames(d2)))))
      msg <- c(msg, paste(d1.d2, "have different column names."))

    # Check rownames
    rowStatus <- sum(c(is.null(rownames(d1)), is.null(rownames(d2))))
    if ((rowStatus == 1) | ((rowStatus == 0) & (any(rownames(d1) != rownames(d2)))))
      msg <- c(msg, paste(d1.d2, "have different row names."))

    # if they are data frames, change factors to characters
    if (is.data.frame(d1))
      d1 <- factor2character(d1)
    if (is.data.frame(d2))
      d2 <- factor2character(d2)

    # Change character NA's to special characters "9999999"
    changeNAchar <- function(x)
      ifelse(is.na(x), "999999999999", as.character(x))

    # Function for checking equivalence of numerical values
    cNum <- function(vec1, vec2) {

      # Check NA's
      v1NA <- is.na(vec1)
      v2NA <- is.na(vec2)

      # If only 1 is NA, set them to different numbers
      if (any(oneNA <- v1NA | v2NA)) {
        vec1[oneNA] <- 9999999
        vec2[oneNA] <- 9

        # If they're both NA, set them equal to the same number
        if (any(bothNA <- v1NA & v2NA)) {
          vec1[bothNA] <- 99999
          vec2[bothNA] <- 99999
        }
      }

      # Check +Inf
      v1Inf <- vec1 == Inf
      v2Inf <- vec2 == Inf

      # If only 1 is Inf, set them to different numbers
      if (any(oneInf <- v1Inf | v2Inf)) {
        vec1[oneInf] <- 9999999
        vec2[oneInf] <- 9

        # If they're both Inf, set them to the same number
        if (any(bothInf <- v1Inf & v2Inf)) {
          vec1[bothInf] <- 99999
          vec2[bothInf] <- 99999
        }
      }

      # Check -Inf
      v1nInf <- vec1 == -Inf
      v2nInf <- vec2 == -Inf

      # If only 1 is -Inf, set them to different numbers
      if (any(onenInf <- v1nInf | v2nInf)) {
        vec1[onenInf] <- 9999999
        vec2[onenInf] <- 9

        # If they're both -Inf, set them to the same number
        if (any(bothnInf <- v1nInf & v2nInf)) {
          vec1[bothnInf] <- 99999
          vec2[bothnInf] <- 99999
        }
      }


      # Perfect equivalence
      e1 <- vec1 == vec2

      # Absolute error equivalence
      e2 <- abs(vec1 - vec2) < maxAbsError

      # Relative error equivalence
      e3 <- ifelse(abs(vec2) > abs(vec1),
                   abs((vec1 - vec2) / vec2) < maxRelError,
                   abs((vec1 - vec2) / vec1) < maxRelError)

      # convert any NAs or NaN's to FALSE
      e3[is.na(e3)] <- FALSE

      # Checking for NA's in e1 and e2
      if (any(is.na(e1)))
        warning("in cNum() in dframeEquiv():  'e1' contains an unexpected NA\n", call. = FALSE)
      if (any(is.na(e2)))
        warning("in cNum() in dframeEquiv():  'e2' contains an unexpected NA\n", call. = FALSE)

      # If we have one of the 3 equivalences, return TRUE
      return(e1 | e2 | e3)

    } # cNum

    # Check each column for equivalence
    for (i in 1:NCOL(d1)) {

      v1 <- d1[,i]
      v2 <- d2[,i]

      both.num <- sum(c(is.numeric(v1), is.numeric(v2)))

      if (both.num == 2)
        equiv.mat <- cbind(equiv.mat, cNum(v1,v2))

      else {
        v1 <- changeNAchar(v1)
        v2 <- changeNAchar(v2)
        equiv.mat <- cbind(equiv.mat, v1 == v2)

## This doesn't catch all the different ways in which two columns could have different data types
#        if (both.num == 1)
#          msg <- c(msg, paste("Column", i, "of", d1.d2, "do not have same data type.",
#                   "Will coerce them to characters in order to compare them.")
      }
    } # for (i

    # Fraction of elements that are equivalent
    frac.equiv <- sum(equiv.mat)/(NROW(d1)*NCOL(d1))

    if (!all(equiv.mat)) {
        msg <- c(msg, paste(round(100*frac.equiv,2),
                            "% of the elements of ", d1.d2, " are equivalent.", sep=""))

      # Get the row,column locations of inequivalencies
      locations <- which(!equiv.mat)
      loc.inequiv <- data.frame(row.loc = ifelse(locations %% NROW(d1) == 0,
                                                 NROW(d1), locations %% NROW(d1)),
                                col.loc = ifelse(locations %% NROW(d1) == 0,
                                                 locations %/% NROW(d1),
                                                 (locations %/% NROW(d1)) + 1))
      keep.checking <- FALSE
    }
    else
      msg <- c(msg, paste("All elements of", d1.d2, "appear to be equivalent."))

  }

  if (verbose)
    cat(paste(msg, "\n", sep=""))

  invisible(list(equiv=keep.checking, msg=msg, frac.equiv=frac.equiv,
                 loc.inequiv=loc.inequiv, equiv.matrix=equiv.mat))

} # dframeEquiv

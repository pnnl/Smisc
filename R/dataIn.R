##' A flexible way to import data into R.
##'
##' Imports .Rdata, .csv, package data sets, and regular data frames
##' This is expecially useful when a function requires data as an argument--and
##' in some cases the data frame already exists as an object, ready to be
##' passed into the function, but in other cases it may be more convenient to
##' read the data from a file.
##'
##' @export
##' @param data Can be a data frame or a list of data frames (in which case,
##' the same data frame or list is simply returned), or one of the following
##' types of single text strings: (1) the name of a .csv file, (2) the name of
##' a .Rdata file, or (3) a data set in a particular package, using the syntax
##' "packageName::dataSetName".
##'
##' @return A data frame (or list of data frames) containing the requested
##' data.
##'
##' @author Landon Sego
##'
##' @seealso
##' \code{\link{data}},\code{\link{loadObject}},\code{\link{read.csv}}
##'
##' @keywords misc
##'
##' @examples
##' # Write a simple data set
##' some.data <-data.frame(a=rnorm(10), b=rpois(10, 5))
##' write.csv(some.data, file="tmp.file.csv", row.names=FALSE)
##' save(some.data, file="tmp.file.Rdata")
##'
##' A <- dataIn("tmp.file.csv")
##' B <- dataIn("tmp.file.Rdata")
##' C <- dataIn(some.data)
##'
##' # We expect these to be equivalent (this should be TRUE)
##' all(c(dframeEquiv(A, B, verbose=FALSE)$equiv,
##'       dframeEquiv(B, C, verbose=FALSE)$equiv,
##'       dframeEquiv(A, C, verbose=FALSE)$equiv))
##'
##' # Delete the files
##' unlink(c("tmp.file.csv", "tmp.file.Rdata"))
##'
##' # Loading data from a package
##' more.data <- dataIn("datasets::AirPassengers")
##' print(more.data)
##'
##' # remove example objects
##' rm(A, B, C, more.data, some.data)
##'
##'
dataIn <- function(data) {

  # data can be: a .csv or .Rdata file, a data frame, or a textstring indicating a data object in a particular package, using the
  # nomenclature "packageName::dataSetName".

  fail <- FALSE

  # If 'data' is a character string, then read or load the file
  if (is.character(data)) {

    if (length(data) == 1) {

      # If it has a ::, load the data from the package
      if (grepl("::", data)) {

        splt <- strsplit(data, "::")
        pkg <- splt[[1]][1]
        dat <- splt[[1]][2]

        e1 <- new.env()
        val <- data(list = dat, package = pkg, envir = e1)
        d <- get(val, envir = e1)

      }

      # otherwise, it's a file
      else {

        ext <- tolower(getExtension(data))

        if (ext == "csv")
          d <- read.csv(data)

        else if (ext == "rdata") {
          d <- loadObject(data)
        }

        else
          fail <- TRUE
      }

    } # if (length(data) == 1)

    else
      fail <- TRUE

  }

  # Otherwise, verify it's a data frame
  else if (is.data.frame(data))
    d <- data

  # Otherwise, it should be a list of data frames
  else if (is.list(data)) {
    if (all(unlist(lapply(data, is.data.frame))))
      d <- data
    else
      fail <- TRUE
  }

  else
    fail <- TRUE

  if (fail)
    stop("'data' must be a data frame, a list of data frames, or a single text string specifying\n",
         "either a valid .Rdata or .csv filename or a data set in a package using the syntax\n",
         "'packageName::dataSetName'.\n")

  # return the data frame
  return(d)

} # dataIn

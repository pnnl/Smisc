##' Subtracts two time series by matching irregular time indexes
##'
##' Subtracts two time series by matching irregular time indexes.  Can also be
##' used to align the indexes of a time series to a set of standard
##' time indexes.
##'
##' The format for the timestamps (in the vector names) can be virtually any
##' reasonable format, which will be converted to POSIXct by
##' \code{\link{formatDT}}.
##'
##' Suppose that \code{v1} is shorter than \code{v2}.  For each index of the
##' \code{v1}, the \code{n.ind} indices of \code{v2} that are closest in time
##' to the \code{v1} index are identified and "matched" (by averaging them) to
##' the \code{v1} index. In the case of ties, for example, the nearest
##' \code{v2} indexes are both 5 seconds before and 5 seconds after the
##' \code{v1} index of interest, then the closest \code{v2} index in the past
##' (5 seconds before) is matched to the \code{v1} index.
##'
##' Hence, the average of the \code{n.ind} elements of \code{v2} that best
##' "match" (are closest in time to) the element of \code{v1} are subtracted
##' from \code{v1}, which creates a series of differences with the same length
##' (and timestamps) as \code{v1}.
##'
##' If instead, \code{v2} is shorter than \code{v1}, then the time stamps of
##' \code{v2} become the 'standard' to which the times of \code{v1} are
##' matched.
##'
##' If \code{v1} and \code{v2} are the same length, then the timestamps of
##' \code{v2} are matched to \code{v1} and the resulting vector of differences
##' has the same timestamps as \code{v1}.
##'
##' @export
##' @param v1 A time series vector: vector with dates or datetimes for names
##' which are non repeating and in chronological order
##'
##' @param v2 Another time series vector
##'
##' @param n.ind An integer >= 1 that indicates how many elements of the longer
##' of the two vectors will be averaged and matched to the closest timestamp of
##' the shorter vector.  See details below.
##'
##' @param full \code{=TRUE} returns a data frame that shows in detail how the
##' two vectors were matched and the difference calculated
##'
##' @return if \code{full = FALSE} then the difference (after matching) of
##' \code{(v1 - v2)} is returned. Otherwise, a data frame is returned that
##' shows how the vectors were matched and the resulting difference vector.
##'
##' @author Landon Sego
##'
##' @keywords misc
##'
##' @examples
##' data(timeDiff.eg)
##'
##' # Show the objects
##' print(timeDiff.eg)
##'
##' # Extract the objects from the list for easier use in the example
##' sepList(timeDiff.eg)
##' 
##' # Print warnings as they occur
##' op <- options(warn = 1)
##'
##' # Show various differences
##' timeDiff(x1, x2, full = TRUE)
##' timeDiff(x2.d, x1.d, full = TRUE)
##' timeDiff(x1, x1)
##'
##' options(op)
##'
##' ### If we need to average a time-series at 30 second invervals:
##'
##' # Create the vector that will be averaged, with time stamps occuring
##' # about every 10 seconds
##' v1.names <- seq(formatDT("2009-09-12 3:20:31")$dt.posix,
##'                 formatDT("2009-09-12 3:29:15")$dt.posix, by = 10)
##'
##' # Now jitter the times a bit and look at the time spacing
##' v1.names <- v1.names + round(rnorm(length(v1.names), sd = 1.5))
##' diff(v1.names)
##'
##' # Create the vector
##' v1 <- abs(rnorm(length(v1.names), mean = 7, sd = 3))
##' names(v1) <- v1.names
##'
##' # Now create a standard vector with values of 0 with time stamps every 30 seconds
##' standard.names <- seq(formatDT("2009-09-12 3:21:30")$dt.posix,
##'                       formatDT("2009-09-12 3:28:30")$dt.posix, by = 30)
##' standard <- double(length(standard.names))
##' names(standard) <- standard.names
##'
##' # Now average the v1 values by matching the 3 closest values to each standard time:
##' timeDiff(v1, standard, n.ind = 3, full = TRUE)
##' v1.avg <- timeDiff(v1, standard, n.ind = 3)
##'
##' # Check that every 3 obs were averaged
##' v1.avg.check <- tapply(v1[6:50], rep(1:15, each = 3), mean)
##' max(abs(v1.avg.check - v1.avg))
##'
##'
timeDiff <- function(v1, v2, n.ind = 1, full = FALSE) {

  if ((n.ind < 1) | (n.ind%%1 != 0))
    stop("'n.ind' must be an integer >= 1")

  # Identify the shorter and longer of the sequences
  shorter <- ifelse(length(v1) <= length(v2), "v1", "v2")
  longer <- ifelse(length(v1) > length(v2), "v1", "v2")

  # Sanity check
  if (any(sort(c(shorter,longer)) != c("v1","v2")))
    stop("timeDiff(), Failure 1:  'shorter' and 'longer' mapping failed\n")

  # Get the full vectors
  v.long <- get(longer)
  v.short <- get(shorter)

  # v1 and v2 must be time series (named vectors in chronological order, the names being the time stamps)
  v.long.n <- names(v.long)
  v.short.n <- names(v.short)

  # Verify there are names
  if (is.null(v.long.n) | is.null(v.short.n))
    stop("timeDiff(), Failure 2:  v1 and v2 must be named vectors, with date or datetimes as the names\n")

  # Convert dates or datetimes to posix (this will verify the names are date times)
  v.long.fdt <- formatDT(v.long.n)
  v.short.fdt <- formatDT(v.short.n)

  # If both are not date times, there should be dates only
  ldtp <- is.null(v.long.fdt$dt.posix)
  sdtp <- is.null(v.short.fdt$dt.posix)
  if (ldtp & sdtp)
    var <- "date.posix"

  # Otherwise, datetimes should exist in both
  else if (!ldtp & !sdtp)
    var <- "dt.posix"

  # Otherwise they are both not the same type
  else
    stop("timeDiff(), Failure 3:  names of v1 and v2 must both be datetimes or both be dates only\n")

  # Select out the datetime or date element from the list
  v.long.t <- v.long.fdt[[var]]
  v.short.t <- v.short.fdt[[var]]

  # Dates or dateimes must be increasing and non-repeating
  if (any(diff(v.long.t) < 0) | any(diff(v.short.t) < 0))
    stop("timeDiff(), Failure 4:  Names of v1 and v2 must be chronological (increasing) date times\n")

  # Environment for recording selected indexes
  subEnv <- new.env()
  assign("index.vec", NULL, envir = subEnv)
  assign("t.index.vec", NULL, envir = subEnv)

  # Function to identify the index that is closest to time stamp
  # As a convention, if there is a tie, then choose the closest one in the past (which would be positive)
  id.index <- function(x) {

    # Calculate the vector of differences, in seconds (if a datetime) or in days (if a date)
    ddiff <- data.frame(names = 1:length(v.long.t),
                        v.diff = as.numeric(x - v.long.t),
                        a.v.diff = abs(as.numeric(x - v.long.t)))

    # Sort first by the absolute difference, then by descending difference (so that postive values come first on ties)
    ddiff <- ddiff[order(ddiff$a.v.diff, -ddiff$v.diff), ]
#    ddiff <- sortDF(ddiff, ~ a.v.diff - v.diff)

    # Will select 1:n.ind of the min.indexes--but we need to make sure that n.ind is not tied with others below it
    if (any(ties <- ddiff[(n.ind+1):NROW(ddiff), "a.v.diff"] == ddiff[n.ind, "a.v.diff"])) {

      if (sum(ties) > 1)
        stop("id.index():  Only 1 tie is expected")

      if (any(ties[-1]))
        stop("id.index(): Only expecting a tie to occur on the n.ind+1 index")

      # Tie values:  One should be negative, one should be positive (should sum to near 0)
      if (abs(sum(ddiff[c(n.ind, n.ind+1), "v.diff"])) > 1e-08)
        stop("id.index(): Tie values did not sum to 0")

      # Verify the n.ind value is positive
      if (ddiff[n.ind, "v.diff"] < 0)
        stop("id.index(): ddiff[n.ind, 'v.diff'] < 0")

    } # if any ties


    # Get numerical indexes of the date-times that will be returned (they should be consecutive)
    indexes <- sort(ddiff[1:n.ind, "names"])

    if (any(diff(indexes) != 1))
      stop("id.index(): Selected indexes are not contiguous")

    # For checking:
#    pvar(x, indexes)
#    pvar(names(v.long)[indexes])

    # Accumulate the indexes
    assign("index.vec", c(get("index.vec", envir = subEnv), indexes), envir = subEnv)
    assign("t.index.vec", c(get("t.index.vec", envir = subEnv), paste(indexes, collapse = ",")), envir = subEnv)

    # Return the mean of the times
    return(mean(v.long[indexes]))

  } # id.index


  # Vector of matched names
  matched.means <- unlist(tapply(v.short.t, v.short.n, id.index))

  # Look for overlapping windows from n.ind being too large
  selected.indexes <- get("index.vec", envir = subEnv)
  text.selected.indexes <- get("t.index.vec", envir = subEnv)

  if ((n.ind > 1) & any(diff(selected.indexes) <= 0))
    warning(pvar(n.ind, verbose = FALSE), " may be too large. Averaging windows may be overlapping or values\n",
            "of the longer vector may be matched (averaged) to more than one index of the shorter vector")

##   cat("\n")
##   pvar(check.indexes)
##   pvar(diff(check.indexes))
##   cat("\n")


  # The names and lengths of the two vectors should be the same
  if (length(matched.means) != length(v.short))
    stop("timeDiff() Failure 5: length(matched.means) != length(v.short)\n")

  # Sanity check
  if (!all(names(matched.means) == v.short.n))
    stop("id.index() in timeDiff(), Failure 3:  names aren't matching\n")

  # Now calculate the difference v1 - v2  (this works if v.short is v1)
  tdiff <- v.short - matched.means
  names(tdiff) <- v.short.n

  # Change the sign if v.short is v2
  if (shorter == "v2")
    tdiff <- -1 * tdiff

  # Create a dataframe which illustrates the matching
  if (full) {

    # Create the dataframe
    show.match <- data.frame(v.short.t = v.short.t,
                             v.long.t = NA,
                             original.index = text.selected.indexes,
                             v.short = v.short,
                             new.v.long = matched.means,
                             diff = tdiff)

    if (n.ind == 1)
      show.match[,"v.long.t"] <- v.long.t[as.numeric(text.selected.indexes)]

    # Rename the columns with names that match the original input objects
    dv1 <- deparse(substitute(v1))
    dv2 <- deparse(substitute(v2))
    s.name <- ifelse(shorter == "v1", dv1, dv2)
    l.name <- ifelse(longer == "v1", dv1, dv2)
    i.name <- paste("original", l.name, "index", sep = ".")
    t.name <- paste(stripExtension(i.name), "time", sep = ".")
    if (n.ind > 1)
      l.name <- paste(l.name, "average", sep = ".")

    colnames(show.match) <- c(paste(s.name, "timestamps", sep = "."),
                              t.name,
                              i.name,
                              s.name,
                              l.name,
                              paste("diff", dv1, dv2, sep = "."))

    rownames(show.match) <- 1:NROW(show.match)

    return(show.match)

  }
  else
    return(tdiff)

} # timeDiff

# Snaps the time stamps of two series together, optionally identifies the amount of time they are furthest apart
# Returns the difference of the two vectors.

timeDiff <- function(v1, v2, n.ind=1, full=FALSE) {

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
  assign("index.vec", NULL, envir=subEnv)
  assign("t.index.vec", NULL, envir=subEnv)
  
  # Function to identify the index that is closest to time stamp
  # As a convention, if there is a tie, then choose the closest one in the past (which would be positive)
  id.index <- function(x) {
    
    # Calculate the vector of differences, in seconds (if a datetime) or in days (if a date)
    ddiff <- data.frame(names = 1:length(v.long.t),
                        v.diff = as.numeric(x - v.long.t),
                        a.v.diff = abs(as.numeric(x - v.long.t)))

    # Sort first by the absolute difference, then by descending difference (so that postive values come first on ties)
    ddiff <- sort.data.frame(ddiff, ~ a.v.diff - v.diff)
    
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
    assign("index.vec", c(get("index.vec", envir=subEnv), indexes), envir=subEnv)
    assign("t.index.vec", c(get("t.index.vec", envir=subEnv), paste(indexes, collapse=",")), envir=subEnv)
    
    # Return the mean of the times
    return(mean(v.long[indexes]))

  } # id.index
      

  # Vector of matched names
  matched.means <- unlist(tapply(v.short.t, v.short.n, id.index))

  # Look for overlapping windows from n.ind being too large
  selected.indexes <- get("index.vec", envir=subEnv)
  text.selected.indexes <- get("t.index.vec", envir=subEnv)
  
  if ((n.ind > 1) & any(diff(selected.indexes) <= 0))
    warning(pvar(n.ind, verbose=FALSE), " may be too large. Averaging windows may be overlapping or values\n",
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
    i.name <- paste("original", l.name, "index", sep=".")
    t.name <- paste(stripExtension(i.name), "time", sep=".")
    if (n.ind > 1)
      l.name <- paste(l.name, "average", sep=".")
  
    colnames(show.match) <- c(paste(s.name, "timestamps", sep="."),
                              t.name,
                              i.name,
                              s.name,
                              l.name,
                              paste("diff", dv1, dv2, sep="."))

    rownames(show.match) <- 1:NROW(show.match)

    return(show.match)

  }
  else
    return(tdiff)

} # timeDiff

##' Convert a list to a data frame
##'
##' Convert a list of vectors (or data frames) with same numbered lengths (or
##' number of columns) into a data frame.
##'
##' If the elements of \code{vList} are vectors, each vector must have the same
##' length, \code{n}, and the resulting data frame will have \code{n} columns.
##' If the elements of \code{vList} are data frames, each data frame must have the
##' same structure (though they may have differing numbers of rows).
##' If the elements of \code{vList} are lists, each list is first converted to a data frame
##' via \code{\link{as.data.frame}} and the resulting data frames must have the
##' same structure (though they may have differing numbers of rows).
##'
##' It is permissible for \code{vList} to contain \code{NULL} elements.
##' \code{list2df} performs numerous consistency checks to ensure that contents
##' of \code{vList} which are combined into the resulting data frame are
##' conformable, labeled consistently, of the equivalent class when necessary,
##' etc.
##'
##' @export
##' 
##' @param vList List of vectors, data frames, or lists. See Details.
##'
##' @param col.names Optional character vector of length \code{n} with column
##' names that will be given to the output data frame.  If \code{col.names =
##' NULL}, column names are extracted if possible from the column names (or
##' names) of the data frames (or vectors).
##'
##' @param row.names Optional character vector with length equivalent to the
##' length of \code{vList} containing the row names of the output data frame.
##' If \code{row.names = NULL}, row names from the data frames (or names of the
##' \code{vList} elements) if possible.
##'
##' @param convert.numeric If \code{vList} is list of vectors, \code{= TRUE}
##' attempts to convert each column to numeric if possible using
##' \code{\link{as.numericSilent}}
##'
##' @param strings.as.factors If \code{vList} is a list of vectors or lists, \code{=
##' FALSE} converts factors into characters using \code{\link{factor2character}}.
##'
##' @return If \code{vList} is list of data frames, a data frame resulting from
##' efficiently row binding the data frames in \code{vList} is returned.  If
##' \code{vList} is a list of vectors, a data frame is returned where the first
##' column contains the first elements of the list vectors, the second column
##' contains the second elements of the list vectors, etc.
##'
##' @author Landon Sego
##'
##' @examples
##' # For a list of vectors
##' x <- c("r1c1 1", "r2c1 2", "r3c1 3", "r4c4 4")
##' y <- strsplit(x, "\ ")
##' y
##' list2df(y)
##' list2df(y, col.names = LETTERS[1:2])
##'
##' # Here's another list of vectors
##' z <- list(NULL, a = c(first = 10, second = 12), NULL, b = c(first = 15, second = 17))
##' z
##' list2df(z)
##'
##' # For a list of data frames
##' z <- list(d1 = data.frame(a = 1:4, b = letters[1:4]),
##'           d2 = data.frame(a = 5:6, b = letters[5:6]))
##' z
##' list2df(z)
##'
##' # A list of lists
##' z <- list(list(a = 10, b = TRUE, c = "hi"), list(a = 12, b = FALSE, c = c("there", "bye")))
##' z
##' list2df(z)
##'
list2df <- function(vList, col.names = NULL, row.names = NULL, convert.numeric = TRUE,
                    strings.as.factors = FALSE) {

  # Assume list is balanced.  Check for it
  lengths <- unlist(lapply(vList, function(x) ifelse(is.null(x), -1, length(x))))
  not.NULL.indicator <- lengths > -1

  # Verify that all non-NULL list elements have the same class
  firstClass <- class(vList[not.NULL.indicator][[1]])
  if (!all(unlist(lapply(vList[not.NULL.indicator], function(x) all(class(x) == firstClass)))))
    stop("All elements of the list must have the same class\n")

  # Get the index of the first non null element
  first.non.NULL <- which(not.NULL.indicator)[1]

  # Estimate of the standard element length
  element.length <- lengths[first.non.NULL]

  if (!all(element.length == lengths[not.NULL.indicator])) {
    stop("List must be balanced. Each list element must have the same\n",
         "number of columns (for data frames) or the same length (for vectors)\n")
  }

  # If all the elements are lists, convert them to data frames
  if (all(unlist(lapply(vList, function(x) is.list(x) | is.null(x))))) {
    vList <- lapply(vList, function(x) if (!is.null(x)) as.data.frame(x) else NULL)
  }

  # See if list elements are data frames
  if (all(unlist(lapply(vList, function(x) is.data.frame(x) | is.null(x))))) {

    # Make sure all the column names are the same
    if (!all(apply(matrix(unlist(lapply(vList, colnames)), ncol = NCOL(vList[[first.non.NULL]]),
                          byrow = TRUE),
                   2, function(x) {length(unique(x)) == 1})))
      stop("Not all the column names are the same in the data frames")

    # Make sure the classes are the same
    # Extract the classes of each variable into a matrix of class types
    class.m <- matrix(unlist(lapply(vList, function(x) unlist(lapply(x, function(x) class(x)[1])))),
                      ncol = NCOL(vList[[first.non.NULL]]), byrow = TRUE)

    if (!all(apply(class.m, 2, function(x) {length(unique(x)) == 1})))
      stop("Not all the variables (columns) in the data frames have the same class")

    # Now rbind all the data frames together
    text.to.eval <- paste("rbind(",
                          paste(paste("vList[[", c(1:length(vList))[not.NULL.indicator], "]]", sep=""),
                                collapse=","), ")", sep="")

    out <- eval(parse(text = text.to.eval))

    # Apply column names if asked
    if (!is.null(col.names)) {

      if (length(col.names) != NCOL(out))
        warning("The number of column names provided does not match the number of columns\n",
                "in the data frames.  Requested column names were not assigned.\n")
      else
        colnames(out) <- col.names
    }

    # Apply row names
    if (!is.null(row.names)) {

      if (NROW(out) != length(row.names)) {

        extra.text <- ifelse(!all(not.NULL.indicator),
                             paste("\nNote that there were", sum(!not.NULL.indicator),
                                   "NULL elements in 'vList'\n"),
                             "")

        warning(pvar(length(row.names), verbose = FALSE),
                " does not match the number of rows in the\noutput dataframe, ",
                pvar(NROW(out), verbose = FALSE), ". Rownames were not assigned.",
                extra.text)
      }
      else
        rownames(out) <- row.names

    } # if row.names is NULL

  } # if a data frame

  # else if not data frames
  else {

    # Otherwise, they need to be vectors
    if (!all(unlist(lapply(vList, function(x) is.vector(x) | is.null(x))))) {
      stop("All subelements in the list must be vectors\n")
    }

    # Check for names and whether they're all the same
    if (is.null(col.names)) {

      # Default col.names
      col.names <- paste("V", 1:element.length, sep = "")

      # If names are not NULL
      if (!is.null(uNames <- unlist(nList <- lapply(vList, names)))) {

        # If the names are all the same
        if (all(apply(matrix(uNames, ncol = element.length, byrow = TRUE), 2,
                      function(x) length(unique(x))) == 1))
          col.names <- nList[[first.non.NULL]]
        else
          warning("Names are not the same for each element in vList")

      } # if names are not null

    }  # if (is.null(col.names))
    else if (length(col.names) != element.length)
      stop("Length of 'col.names' does not match the length of each vector in the list\n")

    # If the vectors are all numeric, populate using a matrix
    if (all(unlist(lapply(vList, function(x) is.numeric(x) | is.null(x))))) {

      out <- as.data.frame(matrix(unlist(vList), byrow = TRUE, ncol = element.length))
      colnames(out) <- col.names

    }
    # Vectors are not numeric (likely character)
    else {

      # While just populating a matrix with the unlisted list would be more efficient, this
      # allows us to create different data types for each variable
      for (i in 1:element.length) {

        extract.i <- unlist(lapply(vList, function(x) x[i]))

        if (convert.numeric) {
          extract.i <- as.numericSilent(extract.i)
        }

        assign(paste("V", i, sep = ""), extract.i)
      }

      # Text to make the data frame
      text.df <- paste("data.frame(", paste(paste(col.names, "=", paste("V", 1:element.length, sep = "")),
                                            collapse = ", "),
                       ")", sep="")


      # Build the data frame
      out <- eval(parse(text = text.df))

    } # else vectors not numeric

    # Get rownames from the list if they were not supplied
    if (is.null(row.names)) {
      row.names <- names(vList)[not.NULL.indicator]
    }

    # Otherwise, make sure the length of the rownames matches the length of vList
    else if (length(row.names) != length(vList)) {

      warning(pvar(length(row.names), verbose = FALSE),
              " does not match the number of non NULL elements in 'vList':\n",
              pvar(length(vList[not.NULL.indicator]), verbose = FALSE),
              ". Rownames were not assigned.\n")

      row.names <- NULL
    }

    # Attempt to apply the row names if they are present
    if (!is.null(row.names)) {
      rownames(out) <- row.names
    }

    # Convert factors to characters if requested
    if (!strings.as.factors) {
      out <- factor2character(out)
    }

  } # else

  return(out)

} # list2df


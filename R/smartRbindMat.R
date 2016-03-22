##' Row bind matrices whose column names may not be the same
##'
##' @export
##' @param \dots matrix objects (separated by commas), a list of matrices, or a
##' character vector containing the names of matrix objects
##'
##' @param distinguish if \code{TRUE}, then rownames of the returned matrix are
##' assigned a name consisting of the source object name as a prefix, followed by the
##' row name, separated by a ":".  Otherwise, the original rownames are used.
##'
##' @param filler The character to insert into the final matrix for those empty
##' elements which occur when not all the matrices have the same column names.
##'
##' @return Produces a matrix with a union of the column names.  Empty elements
##' resulting from different column names are set to the value of
##' \code{filler}.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{rbind}}
##'
##' @keywords misc
##'
##' @examples
##' x <- matrix(rnorm(6), ncol = 2, dimnames = list(letters[1:3],letters[4:5]))
##' y <- matrix(rnorm(6), ncol = 3, dimnames = list(letters[7:8],letters[4:6]))
##' z <- matrix(rnorm(2), nrow = 1, dimnames = list("c",letters[3:4]))
##'
##' x
##' y
##' z
##'
##' smartRbindMat(x,y,z)
##' smartRbindMat(list(x, y, z), distinguish = TRUE)
##' smartRbindMat(y,z,x, distinguish = TRUE)
##' smartRbindMat(c("y","x","z"), filler = -20, distinguish = TRUE)
##'
##' w1 <- matrix(sample(letters[1:26], 6), ncol = 2,
##'              dimnames = list(c("3", "", "4"), c("w", "v")))
##' x1 <- matrix(sample(letters[1:26], 6), ncol = 2,
##'              dimnames = list(NULL, letters[4:5]))
##' y1 <- matrix(sample(letters[1:26], 6), ncol = 3,
##'              dimnames = list(NULL, letters[4:6]))
##' z1 <- matrix(sample(letters[1:26], 2), nrow = 1,
##'              dimnames = list(NULL, letters[3:4]))
##'
##' w1
##' x1
##' y1
##' z1
##'
##' smartRbindMat(w1,x1,y1,z1)
##' smartRbindMat(list(w1 = w1, x1 = x1, y1 = y1, z1 = z1), distinguish = TRUE)
##'
##' smartRbindMat(w1,x1,y,z1,z)
##' smartRbindMat(w1,x1,y,z1,z, distinguish = TRUE)
##'
smartRbindMat <- function(..., distinguish = FALSE, filler = NA) {

  # Gather key pieces
  matrices <- list(...)
  obj.names <- as.character(substitute(list(...)))[-1]
  mats <- matrices
  names(mats) <- obj.names

  # If matrices is a list with 1 element with a single character vector:
  if (length(matrices) == 1) {

    # See what's in the element of the list
    firstElem <- matrices[[1]]

    # Make sure it's not a matrix
    if (!is.matrix(firstElem)) {

      # If it's a character vector
      if (is.vector(firstElem) & is.character(firstElem)) {
        mats <- lapply(as.list(firstElem), get)
        names(mats) <- firstElem
      }

      # If it's a list, then this is what we want
      else if (is.list(matrices)) {

        mats <- firstElem

        # Add names if necessary
        if (is.null(names(mats)))
          names(mats) <- 1:length(mats)

      }
      else
        stop("'...' must be matrix objects, a list of matrices, or a ",
             "character vector whose elements are the names of matrices")

      obj.names <- names(mats)

    } # If it's not a matrix

  } # If it's length 1

  # Verify names of mats aren't duplicated.  If they are, replace them with unique
  # names
  if (any(duplicated(names(mats))))
    names(mats) <- make.unique(names(mats))

  # If any of the objects are NULL, remove them from the list

  # Verify they are all dataframes or matrices, and obtain the union of the
  # column names.  If rownames are missing, they are assigned a number
  # with the object name as a prefix, separated by a ":"
  union.colnames <- NULL
  cnt <- 0

  check.objects <- function(x) {

    # Increment the counter (and record the value outside the function)
    cnt <<- cnt + 1

    # This allows the possibility that one of objects was NULL
    if (!is.null(x)) {

      # Verify we're working with a matrix
      if (!is.matrix(x))
        stop("'", obj.names[cnt], "' is not a matrix\n")

      cnames <- colnames(x)

      # Make sure the names are not missing
      if (is.null(cnames))
        stop("'", obj.names[cnt], "' does not have column names\n")

      if (any(cnames == ""))
        stop("'", obj.names[cnt], "' has empty column name(s)\n")

      # Column names must be unique
      if (length(unique(cnames)) < length(cnames))
        stop("'", obj.names[cnt], "' does not have unique column names\n")

      # Find the union of all the column names, record the value outside the function
      union.colnames <<- union(union.colnames, cnames)

      # Add in rownames if requested
      if (distinguish) {

        if (is.null(rownames(x)))
          rownames(x) <- 1:NROW(x)

        rownames(x) <- paste(obj.names[cnt], rownames(x), sep=":")
      }

    } # if (!is.null(x))

    return(x)

  } # check.objects()

  # Makes it easier to trace an error
  mats <- try(lapply(mats, check.objects), silent = TRUE)
  if (class(mats) == "try-error")
    stop(mats)

  # Add in filler columns where necessary and reorder to give the same ordering
  addFiller <- function(x) {

    # Identify the differences
    missing.cols <- setdiff(union.colnames, colnames(x))

    # Add in missing columns
    if (length(missing.cols)) {
      for (mc in missing.cols) {
        x <- cbind(x, rep(filler, NROW(x)))
        colnames(x)[NCOL(x)] <- mc
      }
    }

    # Prevent matrices from 1 row from being collapsed into a named vector
    if (NROW(x) == 1)
      return(matrix(x[,union.colnames], nrow = 1,
                    dimnames=list(rownames(x), union.colnames)))
    else
      return(x[,union.colnames])

  } # addFiller

  # Makes it easier to trace an error
  mats <- try(lapply(mats, addFiller), silent = TRUE)
  if (class(mats) == "try-error")
    stop(mats)

  # Text that, when executed, will bind all the matrices together with
  # a single call to rbind()
  rbind.text <- paste("rbind(",
                      paste(paste("mats[[", names(mats), "]]", sep='"'),
                            collapse=","),
                      ")", sep="")

  # return the result
  return(eval(parse(text = rbind.text)))

} # smartRbind()


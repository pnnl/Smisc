################################################################################
#
# Filename:   smartRbindMat.R
# Creator:    Landon Sego
# Date Created:  June 2007
# Description/Purpose:  Row bind matrices whose column names may not be the
#                       same.  Produces a matrix with a union of the
#                       column names.  Row names are reproduced if they exist,
#                       otherwise, unique rownames are created.
#
# Modified by:
# Date modified:
# Brief summary of changes:
#
# Modified by:
# Date modified:
# Brief summary of changes:
# 
# Modified by:
# Date modified:
# Brief summary of changes:
#
#
################################################################################

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
    assign("cnt", cnt + 1, inherits = TRUE)

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
      assign("union.colnames", union(union.colnames, cnames), inherits = TRUE)
  
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


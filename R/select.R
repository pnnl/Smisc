##' Select rows or columns from data frames or matrices while always returning a data frame or a matrix
##'
##' The primary contribution of this function is that if a single row or column is selected, the
##' object that is returned will be a matrix or a dataframe---and it will not be collapsed into a
##' single vector, as is the usual behavior in R.
##'
##' Selecting no rows or no columns is possible if \code{selection = 0} or if \code{length(selection) == 0}.
##' In this case, a data frame or matrix with either 0 columns or 0 rows is returned.
##'
##' @export select
##' @param data A matrix or dataframe from whence data will be selected
##'
##' @param selection A character vector with column (or row) names, or, a numeric vector with
##' column (or row) indexes.
##'
##' @param cols A logical, that when \code{TRUE}, indicates that columns will be selected.
##' If \code{FALSE}, rows will be selected.
##'
##' @return The matrix or data frame is returned with the selected columns or rows.
##'
##' @author Landon Sego
##'
##' @examples
##' # Consider this data frame
##' d <- data.frame(a = 1:5, b = rnorm(5), c = letters[1:5], d = factor(6:10),
##'                 row.names = LETTERS[1:5], stringsAsFactors = FALSE)
##'
##' # We get identical behavior when selecting more than one column
##' d1 <- d[, c("d", "c")]
##' d1c <- select(d, c("d", "c"))
##' d1
##' d1c
##' identical(d1, d1c)
##'
##' # Different behavior when selecting a single column
##' d[,"a"]
##' select(d, "a")
##'
##' # We can also select using numeric indexes
##' select(d, 1)
##'
##' # Selecting a single row from a data frame produces results identical to default R behavior
##' d2 <- d[2,]
##' d2c <- select(d, "B", cols = FALSE)
##' identical(d2, d2c)
##'
##' # Now consider a matrix
##' m <- matrix(rnorm(20), nrow = 4, dimnames = list(LETTERS[1:4], letters[1:5]))
##'
##' # Column selection with two or more or more columns is equivalent to default R behavior
##' m1 <- m[,c(4, 3)]
##' m1c <- select(m, c("d", "c"))
##' m1
##' m1c
##' identical(m1, m1c)
##'
##' # Selecting a single column returns a matrix of 1 column instead of a vector
##' m[,2]
##' select(m, 2)
##'
##' # Selecting a single row returns a matrix of 1 row instead of a vector
##' m2 <- m["C",]
##' m2c <- select(m, "C", cols = FALSE)
##' m2
##' m2c
##' is.matrix(m2)
##' is.matrix(m2c)
##'
##' # Selecting no rows or no columns occurs if 0 or an object of length 0
##' # is passed to 'selection'
##' select(d, 0)
##' select(d, which("bizarre" %in% colnames(d)))
##' select(d, 0, cols = FALSE)

select <- function(data, selection, cols = TRUE) {

  # Check types
  stopifnotMsg(is.matrix(data) | is.data.frame(data),
               "'data' should be a matrix or a data frame",
               if (!is.character(selection)) {
                 if (is.numeric(selection)) {
                   if (length(selection)) {
                     all(selection >= 0) & all(selection %% 1 == 0)
                   } else TRUE
                 } else FALSE
               } else TRUE,
               "'selection' should be a character vector, a numeric vector of whole numbers, or the value 0",
               is.logical(cols) & (length(cols) == 1),
               "'cols' should be TRUE or FALSE")

  # Special case of 0 columns or rows being selected
  degenerate <- FALSE

  # These strange conditions are necessary because if selection
  # has 0 length, then testing for 'selection == 0' fails
  if (!length(selection)) {
    degenerate <- TRUE
  }
  else if (length(selection) == 1) {
    if (selection == 0) {
      degenerate <- TRUE
    }
  }

  if (degenerate) {
    if (cols) {
      return(data[,selection])
    }
    else {
      return(data[selection,])
    }
  }

  # Define an error message function if wrong rows or columns are selected
  errMsg <- function(nt) {

    paste(ifelse(cols, "Column", "Row"),
          ifelse(length(nt) > 1, "s '", " '"),
          ifelse(length(nt) <= 5,
                 paste(paste(nt, collapse = "', '"), "' ", sep = ""),
                 paste(paste(nt[1:5], collapse = "', '"), "', and others ", sep = "")),
          ifelse(length(nt) > 1, "are", "is"),
          " not present in 'data'", sep = "")

  } # errMsg

  # Convert characters to indexes if necessary
  if (is.character(selection)) {

    # If we have columns
    if (cols) {

      # Verify column names match
      if (length(notThere <- setdiff(selection, colnames(data)))) {
        stop(errMsg(notThere))
      }
      
      # Get the column indexes. This complicated code preserves the ordering of 'selection'
      cd <- colnames(data)
      selection <- unlist(lapply(selection, function(x) which(cd %in% x)))

    }

    # If we're working with rows
    else {

      # Verify row names match
      if (length(notThere <- setdiff(selection, rownames(data)))) {
        stop(errMsg(notThere))
      }

      # Get the row indexes
      rd <- rownames(data)
      selection <- unlist(lapply(selection, function(x) which(rd %in% x)))

    }
  }

  # Verify that the numeric indexes are in the range of 'data'''
  else {
    if (cols) {
      if (length(notThere <- setdiff(selection, 1:NCOL(data)))) {
        stop(errMsg(notThere))
      }
    }
    else {
      if (length(notThere <- setdiff(selection, 1:NROW(data)))) {
        stop(errMsg(notThere))
      }
    }
  }

  # Variables required to determine the case we're in
  nRow <- nrow(data)
  nCol <- ncol(data)
  lSel <- length(selection)
  
  # Case 1:  n x m, select > 1 row or select > 1 column
  if ((nRow > 1) & (nCol > 1) & (lSel > 1)) {
      
     if (cols) {
       out <- data[,selection]
     }
     else {
       out <- data[selection,]
     }
     
  } # Case 1


  # Case 2:  n x m, select 1 row or 1 column
  else if ((nRow > 1) & (nCol > 1) & (lSel == 1)) {

    # If we're working with matrices
    if (is.matrix(data)) {

      if (cols) {

        # Create a new matrix of one column
        out <- matrix(data[,selection], ncol = 1,
                      dimnames = list(rownames(data), colnames(data)[selection]))

      }
      # Create a matrix of 1 row
      else {

        out <- matrix(data[selection,], nrow = 1,
                      dimnames = list(rownames(data)[selection], colnames(data)))
      }


    } # Working with matrices

    # If we're working with a data frame
    else {

      if (cols) {

        # Determine whether the single column is character
        isChar <- is.character(singleCol <- data[,selection])

        # Create a new data frame with one column
        out <- data.frame(singleCol, row.names = rownames(data), stringsAsFactors = !isChar)
        colnames(out) <- colnames(data)[selection]

      }
      else {

        # Create a new data frame with one row
        out <- data.frame(data[selection,], row.names = rownames(data)[selection])
        colnames(out) <- colnames(data)

      }

    } # Working with data frames

      
  } # Case 2

  # Case 3:  n x 1, select 1 row, select multiple rows, select 1 column
  else if ((nRow > 1) & (nCol == 1)) {

    # Selecting 1 column
    if (cols) {
      out <- data
    }

    # Selecting 1 or multiple rows
    else {

      # Matrixes
      if (is.matrix(data)) {
  
        out <- matrix(data[selection, 1], ncol = 1,
                      dimnames = list(rownames(data)[selection], colnames(data)))
      }
      
      # Data frames
      else {
  
        isChar <- is.character(data[,1])
        out <- data.frame(data[selection, 1], row.names = rownames(data)[selection], stringsAsFactors = !isChar)
        colnames(out) <- colnames(data)
        
      }
        
    }
      
  } # Case 3

  # Case 4:  1 x n, select 1 row, select multiple columns, select 1 column
  else if ((nRow == 1) & (nCol > 1)) {

    # Selecting a single row
    if (!cols) {
      out <- data
    }

    # Selecting 1 or more columns
    else {

      # For a matrix
      if (is.matrix(data)) {

        out <- matrix(data[1, selection], nrow = 1,
                      dimnames = list(rownames(data), colnames(data)[selection]))
            
      }

      # For a data frame
      else {

        out <- data.frame(data[1, selection], row.names = rownames(data))
        colnames(out) <- colnames(data)[selection]

        # If only 1 column was selected, the string will have been converted to a factor
        if ((lSel == 1) & (is.character(data[1, selection]))) {
          out[,1] <- as.character(out[,1])
        }
        
      }

    }

  } # Case 4

  # Case 5:  1 x 1
  else if ((nRow == 1) & (nCol == 1)) {
     out <- data
  }

  # No more cases should remain
  else {
    stop("Unexpected error.  This should not have happened")
  }

  # Return the result
  return(out)

} # select

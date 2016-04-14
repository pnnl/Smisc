##' Validate selected elements from a character vector
##'
##' Validate selected elements from a character vector using a variety of selection mechanisms:  logical, names, or
##' numerical indexes
##'
##' This function is especially useful for selecting rows or columns from data frames, while providing informative error messages
##' if the \code{elements} for selection are specified incorrectly.  
##'
##' @export
##' 
##' @param elements elements to select from \code{cVec}.  Can be a logical vector, a vector of numeric indexes,
##' or a character vector of element names.  Note that logical vectors are not recycled as usual:
##' they must be the same length as \code{cVec}. If \code{elements == NULL}, \code{NULL} is returned.
##'
##' @param cVec A character vector from which to select elements (such as row names or column names)
##'
##' @return A character vector with elements that were selected from \code{cVec} using \code{elements}
##'
##' @examples
##' # Define some "column names"
##' cnames <- letters[1:5]
##' cnames
##'
##' # Select the 1st and 3rd column names using a variety of approaches
##' selectElements(c("a", "c"), cnames)
##' selectElements(c(1, 3), cnames)
##' selectElements(c(TRUE, FALSE, TRUE, FALSE, FALSE), cnames)
##'
##' # Select the 1st, 3rd, and 1st columns
##' selectElements(c("a", "c", "a"), cnames)
##' selectElements(c(1, 3, 1), cnames)
##' 
##' # If you don't want to select any of them
##' selectElements(NULL, cnames)

selectElements <- function(elements, cVec) {

  # Return NULL immediately if elements is NULL
  if (is.null(elements)) {
    return(NULL)
  }

  stopifnotMsg(# elements
               if (is.vector(elements) & (length(elements) > 0)) {
                 if (is.numeric(elements)) {
                    all(elements %% 1 == 0) & all(elements > 0) 
                 } else is.character(elements) | is.logical(elements)
               } else FALSE,
               "'elements' must be a character, logical, or numeric (whole number) vector",

               # cvec
               is.vector(cVec) & is.character(cVec) & (length(cVec) > 0),
               "'cVec' must be a character vector")

  # If elements is character
  if (is.character(elements)) {

     # If it is character, the names must be in the column names
     if (!all(elements %in% cVec)) {

       incorrect <- setdiff(elements, cVec)

       inc <- paste("'", paste(incorrect, collapse = "', '"), "'", sep = "")
       cvvals <- paste("c('", paste(cVec, collapse = "', '"), "')", sep = "")

       stop(inc, if (length(incorrect) > 1) " are " else " is ", "not in ", cvvals) 
       
    }

    return(elements)

  }

  # If it's Boolean
  if (is.logical(elements)) {

    if (length(elements) != length(cVec)) {
      stop("When 'elements' is a logical vector, the length of 'elements' must match the length of 'cVec'")
    }

  }
  # If it's numeric
  else if (is.numeric(elements)) {

    # Otherwise, we should have indexes that are in the set 1:length(cVec)
    if (!all(elements %in% 1:length(cVec))) {

      incorrect <- setdiff(elements, 1:length(cVec))
       
      p1 <- ifelse(length(incorrect) == 1, "","es")
      p2 <- ifelse(length(incorrect) == 1, "is","are")
        
      stop("The following numeric index", p1, " provided to 'elements' ", p2, " outside the range of indexes for 'cVec': ",
           paste(incorrect, collapse = ", "))
    }

  } # If it's numeric

  # Convert to character string
 return(cVec[elements])

} # selectElements

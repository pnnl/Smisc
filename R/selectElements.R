##' Validate selected elements from a character vector
##'
##' Validate selected elements from a character vector using a variety of selection mechanisms:  logical, names, or
##' numerical indexes
##'
##' This function is useful for selecting rows or columns from data frames, while providing informative error messages
##' if the \code{elements} for selection are specified incorrectly.  Note that logical vectors (or binary vectors of 0's
##' and 1's) are not recycled as usual: they must be the same length as \code{cVec}.
##'
##' @export
##' 
##' @param elements elements to select from \code{cVec}.  Can be a logical vector, a vector of 1's and 0's (that is
##' converted to Boolean), a vector of numeric indexes, or a character vector of column names.
##' If \code{elements == NULL}, \code{NULL} is returned.
##'
##' @param cVec A character vector from which to select elements
##'
##' @return A character vector with elements that were selected from \code{cVec} using \code{elements}
##'
##' @examples
##' # Define the column names
##' cnames <- letters[1:5]
##' cnames
##' 
##' # Select the 1st and 3rd column names using a variety of approaches
##' selectElements(c("a", "c"), cnames)
##' selectElements(c(1, 3), cnames)
##' selectElements(c(TRUE, FALSE, TRUE, FALSE, FALSE), cnames)
##' selectElements(c(1, 0, 1, 0, 0), cnames)
##' 
##' # If you don't want to select any of them
##' selectElements(NULL, cnames)

selectElements <- function(elements, cVec) {

  # Return NULL immediately if elements is NULL
  if (is.null(elements)) {
    return(NULL)
  }
    
  # Basic checks
  if (!is.vector(elements)) {
    stop("'", deparse(substitute(elements)),
         "', the object provided to the 'elements' argument of Smisc::selectElements(), is not a vector")
  }
  if (!is.vector(cVec)) {
    stop("'", deparse(substitute(cVec)),
         "', the object provided to the 'cVec' argument of Smisc::selectElements(), is not a vector")
  }
  if (!is.character(cVec)) {
    stop("'", deparse(substitute(cVec)),
         "', the object provided to the 'cVec' argument of Smisc::selectElements(), is not character")
  }    
  
  # If elements is character
  if (is.character(elements)) {

     # If it is character, the names must be in the column names
     if (!all(elements %in% cVec)) {
        stop("Invalid values for 'elements':  '",
             paste(setdiff(elements, cVec), collapse = "', '"),
             "' are not in 'cVec'")
    }

    return(elements)
     
  }

  # For all other formats:
  
  # If it's Boolean
  if (is.logical(elements)) {
  
    if (length(elements) != length(cVec)) {
      stop("When 'elements' is a logical vector, the length of 'elements' must match the length of 'cVec'")
    }
        
  }
  # If it's numeric
  else if (is.numeric(elements)) {
  
    # If we have only 0's and 1's
    if (all(sort(unique(elements))[1:2] == c(0, 1))) {
  
      if (length(elements) != length(cVec)) {
        stop("When 0's and 1's are provided for 'elements', the length of 'elements' must match the length of 'cVec'")
      }
  
      # Select the names
      elements <- as.logical(elements)
          
    }
  
    # If we have non-binary numeric indexes
    else {
  
      # Otherwise, we should have indexes that are in the set 1:length(cVec)
      if (!all(elements %in% 1:length(cVec))) {
        stop("The following numeric indexes provided to 'elements' are outside the range of indexes for 'cVec': ",
             paste(setdiff(elements, 1:length(cVec)), collapse = ", "))
      }
  
    }
        
  } # If it's numeric
  
  # If elements didn't conform
  else {
  
    stop("'elements' must be a character, numeric, or boolean vector, or a vector of 0's and 1's to select column names")
  
  }
       
  # Convert to character string
 return(cVec[elements])
  
} # selectElements

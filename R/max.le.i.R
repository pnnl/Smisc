##' Computes the maximum of the vector up to the current index
##' 
##' For each index in a vector, computes the maximum of the vector from the
##' beginning of the vector up to the current index
##' 
##' @param x A numeric or integer vector
##' @return In the sequence \code{x[1], x[2], ..., x[n]}, \code{max.le.i}
##' returns the vector \code{y} such that for each \code{i = 1,...,n},
##' \code{y[i] = max(x[j]; j = 1,...,i)}
##' @author Landon Sego
##' @keywords misc
##' @examples
##' 
##' max.le.i(1:10)
##' max.le.i(c(1,3,4,5,3,2,5,1,7,8,8,6))
##' max.le.i(c(1,3,4,5,3,2,5,1,7,8,8,6) + runif(12))
##' 
max.le.i <- function(x) {

  n <- length(x)

  if (n == 1)
    return(x)
  
  else if (is.integer(x))
    return(.C("max_le_i_INT",
              as.integer(x),
              as.integer(n),
              y = integer(n))$y)
    
  else if (is.numeric(x))
    return(.C("max_le_i_DOUBLE",
              as.double(x),
              as.integer(n),
              y = double(n))$y)
  else
    stop("'x' must be a numeric or integer vector\n")
  
} # max.le.i

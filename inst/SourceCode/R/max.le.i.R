# In the sequence x[1], x[2], ..., x[n], for each i = 1,...,n,
# compute the vector y[i] = max(x[j], j = 1,...,i)

max.le.i <- function(x) {

  n <- length(x)

  if (n == 1)
    return(x)
  
  else if (is.integer(x))
    return(.C("max_le_i_INT",
              as.integer(x),
              as.integer(n),
              y = integer(n),
              PACKAGE="pnlStat")$y)
    
  else if (is.numeric(x))
    return(.C("max_le_i_DOUBLE",
              as.double(x),
              as.integer(n),
              y = double(n),
              PACKAGE="pnlStat")$y)
  else
    stop("'x' must be a numeric or integer vector\n")
  
} # max.le.i

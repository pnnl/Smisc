# Identify linearly dependent rows or columns in a matrix

# Compare 2 vectors to identify whether
findDepMat <- function(X, rows=TRUE, tol=1e-10) {

  if (!is.matrix(X))
    stop("'X' is not a matrix")
  if (!is.numeric(X))
    stop("'X' is not numeric")

  if (!rows)
    X <- t(X)

  depends <- logical(NROW(X))

  for (i in 1:(NROW(X)-1)) {

    for (j in (i+1):NROW(X)) {

      # Only check it if a dependency has not been established
      if (!depends[j])
        depends[j] <- (max(abs(diff(X[i,] / X[j,]))) < tol)
      
    }

  } # for (i

  # Check with qr
  qr.rank <- qr(X, tol=tol)$rank
  if (sum(!depends) != qr.rank)
    warning("Number of linearly independent rows (or columns), ",
            sum(!depends), ", do not agree with rank of X, ", qr.rank, ".\n")

  return(depends)
  
} # findDepMat



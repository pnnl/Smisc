# A continous version of the binomial cdf
# Based on 'binom.c' in the R source code.
# It just doesn't force x and n to be integers

# Landon Sego, 15 Sep 2008

pcbinom <- function(x, n, p, lower.tail = TRUE, log.p = FALSE) {

  # Most of this code is created to deal with special cases when arguments of x, n, and p that have lengths > 1

  # This function does not recycle vectors in the usual fashion.  It expects lengths of arguments to be 1 and/or one other number
  l.x <- length(x)
  l.n <- length(n)
  l.p <- length(p)

  lens <- NULL

  if (l.x > 1)
    lens <- c(lens, l.x)
  if (l.n > 1)
    lens <- c(lens, l.n)
  if (l.p > 1)
    lens <- c(lens, l.p)

  # If any args have length > 1
  if (!is.null(lens)) {
    # If more than 1 arg has length > 1
    if (length(lens) > 1)  {
      # If those args that have length > 1 are not the same length    
      if (!all(diff(lens) == 0))
        stop("'pcbinom' does not recycle vectors in the usual fashion.\n",
             "It expects lengths of arguments to be 1 and/or one other number.  See 'help(pcbinom)'.\n")
    }
  }

  l.vec <- max(c(l.x, l.n, l.p))

  # Make vectors all same length
  if (l.vec > 1) {
    if (l.x == 1)
      x <- rep(x, l.vec)
    if (l.n == 1)
      n <- rep(n, l.vec)
    if (l.p == 1)
      p <- rep(p, l.vec)
  }
    
  n[abs(n) == Inf] <- NaN
  p[abs(p) == Inf] <- NaN

  n[n < 0] <- NaN
  p[p < 0] <- NaN
  p[p > 1] <- NaN

  bad.ones <- is.na(n) | is.na(p)

  xlt0 <- (x < 0) & (!bad.ones)
  nlex <- (n <= x) & (!bad.ones)

  x[xlt0] <- NA
  x[nlex] <- NA

  # Calculate the continuous binomial probabilities
  probs <- pbeta(p, x + 1, n - x, lower.tail=!lower.tail, log.p=log.p)

  # Switch appropriate probabilities to 0 or 1
  if (!log.p) {
    probs[xlt0] <- 0
    probs[nlex] <- 1
  }
  else {
    probs[xlt0] <- -Inf
    probs[nlex] <- 0
  }

  if (any(is.na(probs)))
    warning("NaNs produced")

  return(probs)

} # pcbinom

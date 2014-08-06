# Function for checking inputs of dkbinom and pkbinom
check.kbinom <- function(x, size, prob) {

  x.name <- deparse(substitute(x))
  
  # Verify x is an integer
  x.good <- FALSE
  if (is.numeric(x)) {
   if (all(x >= 0))
     x.good <- TRUE
  }
  if (!x.good)
    stop("'", x.name, "' should be a vector of non-negative integers")

  # Change it to an integer if necessary
  if (any(as.logical(x %% 1))) {
    warning("non-integer values of '", x.name,
            "' will be set to integers using 'as.integer()'")
    x <- as.integer(x)
  }

  # size and prob should be the same length
  if (length(size) != length(prob))
    stop("'size' and 'prob' should be the same length")

  # Check that size are postive integers
  size.good <- FALSE
  if (is.numeric(size)) {
    if (all(size %% 1 == 0) & all(size > 0))
      size.good <- TRUE
  }
  if (!size.good)
    stop("'size' must be a vector of positive integers")

  # Check on probabilities
  prob.good <- FALSE
  if (is.numeric(prob)) {
    if (all(prob >= 0) & all(prob <= 1))
      prob.good <- TRUE
  }
  if (!prob.good)
    stop("'prob' must be a vector of numeric values in [0, 1]")

  return(x)

} # check.kbinom


dkbinom <- function(x, size, prob, log = FALSE, verbose = FALSE) {

  x <- check.kbinom(x, size, prob)

  # If only one type of variate was requested or if the probs are equal:
  # If the probs are all equal
  if (all(diff(prob) == 0) | (length(prob) == 1))
    return(dbinom(x, sum(size), prob[1], log = log))

  # Calculate the probabilities
  res <- .C("dkbinom",
            as.integer(max(x)),
            as.integer(size),
            as.double(prob),
            as.integer(length(size)),
            as.integer(FALSE),
            as.integer(verbose),
            double(max(x)+1),
            double(max(x)+1),
            out = double(max(x)+1),
            double(1),
            PACKAGE = "pnlStat")[["out"]][x+1]

  if (log)
    res <- log(res)

  return(res)
  
} # dkbinom


pkbinom <- function(q, size, prob, log.p = FALSE, verbose = FALSE) {

  x <- check.kbinom(q, size, prob)
  
  # If only one type of variate was requested or if the probs are equal:
  # If the probs are all equal
  if (all(diff(prob) == 0) | (length(prob) == 1))
    return(pbinom(x, sum(size), prob[1], log.p = log.p))

  res <- .C("dkbinom",
            as.integer(max(x)),
            as.integer(size),
            as.double(prob),
            as.integer(length(size)),
            as.integer(FALSE),
            as.integer(verbose),
            double(max(x)+1),
            double(max(x)+1),
            out = double(max(x)+1),
            double(1),
            PACKAGE = "pnlStat")[["out"]]
  
  res <- cumsum(res)[x+1]
  
  if (log.p)
    res <- log(res)

  return(res)
  
} # pkbinom


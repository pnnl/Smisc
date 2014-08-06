dbb <- function(x, N, u, v) {

  # Convert x and N to integers with warning
  x.r <- round(x, 14)
  if (any((x.r %% 1) != 0))
    warning("Values of x were rounded to nearest integer\n")
  N.r <- round(N, 14)
  if (any((N.r %% 1) != 0))
    warning("Values of N were rounded to nearest integer\n")

  x <- as.integer(round(x))
  N <- as.integer(round(N))
  
  # Verify arguments are legitimate
  if (any(u <= 0))
    stop("u must be > 0")
  if (any(v <= 0))
    stop("v must be > 0")
  if (any(x < 0))
    stop("x must be >= 0")
  if (any(N < 0))
    stop("N must be >= 0")
  if (any(x > N))
    stop("x must be <= N")

  #beta(x+u, N-x+v)/beta(u,v)*choose(N,x)
  choose(N, x) * exp(lgamma(x + u) + lgamma(N - x + v) - lgamma(N + u + v) + lgamma(u + v) - lgamma(u) - lgamma(v))

} # dbb

pbb <- function(q, N, u, v) {

  # Checking and adjusting N
  N.r <- round(N, 14)
  if (any((N.r %% 1) != 0))
    warning("Values of N were rounded to nearest integer\n")
  
  N <- as.integer(round(N))

  # u, and v will get checked in dbb

  
  q <- floor(q)

  # Make a vector of the lengths of each argument
  l.q <- length(q)
  l.N <- length(N)
  l.u <- length(u)
  l.v <- length(v)

  # This function does not recycle vectors in the usual fashion.  It expects lengths of arguments to be 1 and/or one other number
  # The following tests this condition
    
  # Gather the lengths of each argument that length > 1 into a single vector
  lens <- c(l.q, l.N, l.u, l.v)
  lens.gt.1 <- lens[lens > 1]

  # If there are any arguments with length > 1
  if (length(lens.gt.1)) {
    # If there are more than 1 argument with length > 1
    if (length(lens.gt.1) > 1) {
      # If those args that have length > 1 are not the same length
      if (!all(diff(lens.gt.1) == 0))
        stop("'pbb' does not recycle vectors in the usual fashion.\n",
             "It expects the lengths of arguments to be 1 and/or one other number.\n")
    }
  }

  # Find the longest argument
  l.vec <- max(lens)
  
  # Make vectors all same length
  if (l.q == 1)
    q <- rep(q, l.vec)
  if (l.N == 1)
    N <- rep(N, l.vec)
  if (l.u == 1)
    u <- rep(u, l.vec)
  if (l.v == 1)
    v <- rep(v, l.vec)

  out <- double(l.vec)

  # Calculate the results separately for each instance
  for (i in 1:l.vec) {
    if (q[i] < 0)
      out[i] <- 0
    else if (q[i] >= N[i])
      out[i] <- 1
    else
      out[i] <- sum(dbb(0:q[i], N[i], u[i], v[i]))
  }


  # Fix any values that exceed 0 or 1 due to machine error
  out[out > 1] <- 1
  out[out < 0] <- 0
  
  return(out)

} # pbb


qbb <- function(p, N, u, v) {

  # Checking and adjusting N
  N.r <- round(N, 14)
  if (any((N.r %% 1) != 0))
    warning("Values of N were rounded to nearest integer\n")
  
  N <- as.integer(round(N))

  # u, and v will get checked in dbb

  # Check values of p
  if (any(p > 1) | any(p < 0))
    stop("p must be in [0,1]")

  # Make a vector of the lengths of each argument
  l.p <- length(p)
  l.N <- length(N)
  l.u <- length(u)
  l.v <- length(v)

  # This function does not recycle vectors in the usual fashion.  It expects lengths of arguments to be 1 and/or one other number
  # The following tests this condition

  
  # Gather the lengths of each argument that length > 1 into a single vector
  lens <- c(l.p, l.N, l.u, l.v)
  lens.gt.1 <- lens[lens > 1]

  # If there are any arguments with length > 1
  if (length(lens.gt.1)) {
    # If there are more than 1 argument with length > 1
    if (length(lens.gt.1) > 1) {
      # If those args that have length > 1 are not the same length
      if (!all(diff(lens.gt.1) == 0))
        stop("'qbb' does not recycle vectors in the usual fashion.\n",
             "It expects the lengths of arguments to be 1 and/or one other number.\n")
    }
  }

  # Find the longest argument
  l.vec <- max(lens)
  
  # Make vectors all same length
  if (l.p == 1)
    p <- rep(p, l.vec)
  if (l.N == 1)
    N <- rep(N, l.vec)
  if (l.u == 1)
    u <- rep(u, l.vec)
  if (l.v == 1)
    v <- rep(v, l.vec)

  out <- double(l.vec)

  # Calculate the results separately for each instance
  for (i in 1:l.vec) {
    if (p[i] == 1)
      out[i] <- N[i]
    else {
      pp <- cumsum(dbb(0:N[i], N[i], u[i], v[i]))      
      out[i] <- sum(pp < p[i])
    }
  }
    
  return(out)
  
} # qbb

# I Assume these recycle properly?...
rbb <- function(n, N, u, v) {

  # Verify arguments are correct
  if (any(u <= 0))
    stop("u must be > 0")
  if (any(v <= 0))
    stop("v must be > 0")

  N.r <- round(N, 14)
  if (any((N.r %% 1) != 0))
    warning("Values of N rounded to nearest integer\n")

  N <- as.integer(round(N))
  
  p <- rbeta(n, u, v)
  rbinom(n, N, p)
  
} # rbb



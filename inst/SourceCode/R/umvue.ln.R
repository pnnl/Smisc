# Compute UMVUE's mu, sigma, and se(mu) of lognormal data
# Based on Gilbert (1987, p 165-166), equations (13.3, 13.5, and 13.6)

# Landon Sego,  2008-10-13

umvue.ln <- function(x) {

  n <- length(x)

  # Trivial case of 1 datum
  if (n == 1)
    return(c(mu = x, se.mu = NaN, sigma = NaN))

  # Intermediate steps
  y <- log(x)
  y.bar <- mean(y)
  s2.y <- var(y)
  psi.1 <- psi.n.t(n, s2.y/2)
  psi.2 <- psi.n.t(n, s2.y * (n - 2) / (n - 1))
  e2ybar <- exp(2 * y.bar)

  # Return results
  return(c(mu = exp(y.bar) * psi.1,
           se.mu = sqrt(e2ybar * (psi.1^2 - psi.2)),
           sigma = sqrt(e2ybar * (psi.n.t(n, 2 * s2.y) - psi.2))))
  
} # umvue.ln


# Calculate psi_n(t) from equation 13.4 in Gilbert (1987, p 165)
# in order to calculate the UMVUE of log-normal data

# Landon Sego,  2008-10-13

psi.n.t <- function(n, tx, tol=1e-15, verbose=FALSE) {

  .C("psi_n_t",
     as.integer(n),
     as.double(tx),
     as.double(tol),
     as.integer(verbose),
     out = double(1),
     PACKAGE="pnlStat")$out

} # psi.n.t


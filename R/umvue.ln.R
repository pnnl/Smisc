# Compute UMVUE's mu, sigma, and se(mu) of lognormal data
# Based on Gilbert (1987, p 165-166), equations (13.3, 13.5, and 13.6)

# Landon Sego,  2008-10-13



##' Computes UMVUEs of lognormal data
##'
##' Computes UMVU estimates of the mean, the standard error of the mean, and
##' the standard deviation of lognormally distributed data.
##'
##' Calculates equations 13.3, 13.5, and 13.6 of Gilbert (1987).
##'
##' @export
##'
##' @param x Vector of lognormal data
##' @return Returns a named vector with the following components \item{mu}{The
##' UMVUE of the mean} \item{se.mu}{The UMVUE standard error of the mean}
##' \item{sigma}{The UMVUE of the standard deviation}
##' @author Landon Sego
##' @references Gilbert, Richard O. (1987) Statistical Methods for
##' Environmental Pollution Monitoring, John Wiley & Sons, Inc. New York, pp
##' 164-167.
##' @keywords misc
##' @examples
##'
##'
##' # Test from Gilbert 1987, Example 13.1, p 166
##' x <- c(3.161, 4.151, 3.756, 2.202, 1.535, 20.76, 8.42, 7.81, 2.72, 4.43)
##' y <- umvue.ln(x)
##' print(y)
##'
##' # Results from PRO-UCL 4.00.02:
##'
##' # MVU Estimate of Mean			  5.6544289
##' # MVU Estimate of Standard Error of Mean  1.3944504
##' # MVU Estimate of SD			  4.4486438
##'
##' # Compare with Gilbert's results (which have rounding error)
##' mu.1.hat <- round(y[1], 2)
##' s2.mu.1.hat <- round(y[2]^2, 2)
##' s2.1.hat <- round(y[3]^2, 1)
##' pvar(mu.1.hat, s2.mu.1.hat, s2.1.hat)
##'
##'
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
     out = double(1))$out

} # psi.n.t


# Compute UMVUE's mu, sigma, and se(mu) of lognormal data
# Based on Gilbert (1987, p 165-166), equations (13.3, 13.5, and 13.6)

# Landon Sego,  2008-10-13

##' Computes UMVUEs of lognormal parameters
##'
##' Computes uniformly minimum variance unbiased (UMVU) estimates of the mean,
##' the standard error of the mean, and
##' the standard deviation of lognormally distributed data.
##'
##' Calculates equations 13.3, 13.5, and 13.6 of Gilbert (1987).
##'
##' @export
##' @param x Vector of lognormal data
##' 
##' @param tol Tolerence level for convengence of the infinite series, \emph{Psi}. Convergence occurs when the absolute value of the
##' current term in the series is less than \code{tol}.
##' 
##' @param verbose Logical indicating whether iteration steps for convergence of \emph{Psi} are printed.
##'
##' @return Returns a named vector with the following components \item{mu}{The
##' UMVUE of the mean} \item{se.mu}{The UMVUE standard error of the mean}
##' \item{sigma}{The UMVUE of the standard deviation}
##'
##' @author Landon Sego
##'
##' @references Gilbert, Richard O. (1987) Statistical Methods for
##' Environmental Pollution Monitoring, John Wiley & Sons, Inc. New York, pp
##' 164-167.
##'
##' @keywords misc
##'
##' @examples
##'
##' # Test from Gilbert 1987, Example 13.1, p 166
##' x <- c(3.161, 4.151, 3.756, 2.202, 1.535, 20.76, 8.42, 7.81, 2.72, 4.43)
##' y <- umvueLN(x)
##' print(y, digits = 8)
##'
##' # Compare to results from PRO-UCL 4.00.02:
##'
##' # MVU Estimate of Mean                     5.6544289
##' # MVU Estimate of Standard Error of Mean   1.3944504
##' # MVU Estimate of SD                       4.4486438
##'
##' # Compare these to Gilbert's printed results (which have rounding error)
##' Gilbert <- c(5.66, sqrt(1.97), sqrt(19.8))
##' print(round(abs(y - Gilbert), 2))

umvueLN <- function(x, tol = 1e-15, verbose = FALSE) {

  stopifnotMsg(is.numeric(x), "'x' must be numeric",
               if (is.numeric(tol) & length(tol) == 1) {
                 tol > 0
               } else FALSE,
               "'tol' must be a single, small, positive number",
               is.logical(verbose) & length(verbose) == 1,
               "'verbose' must be TRUE or FALSE")
    
  n <- length(x)

  # Trivial case of 1 datum
  if (n == 1)
    return(c(mu = x, se.mu = NaN, sigma = NaN))

  # Intermediate steps
  y <- log(x)
  y.bar <- mean(y)
  s2.y <- var(y)

  # First psi
  if (verbose) {
    cat("psi_n(s^2_y / 2):\n")
  }
  psi.1 <- psi_n_t(n, s2.y/2, tol, verbose)

  # Second psi
  if (verbose) {
    cat("psi_n(s^2_y(n - 2) / (n - 1)):\n")
  }
  psi.2 <- psi_n_t(n, s2.y * (n - 2) / (n - 1), tol, verbose)
  
  e2ybar <- exp(2 * y.bar)

  # Third psi
  if (verbose) {
    cat("psi_n(2 * s^2_y):\n")
  }
  psi.3 <- psi_n_t(n, 2 * s2.y, tol, verbose)
  
  # Return results
  return(c(mu = exp(y.bar) * psi.1,
           se.mu = sqrt(e2ybar * (psi.1^2 - psi.2)),
           sigma = sqrt(e2ybar * (psi.3 - psi.2))))

} # umvueLN


# Calculate psi_n(t) from equation 13.4 in Gilbert (1987, p 165)
# in order to calculate the UMVUE of log-normal data

# Landon Sego,  2008-10-13

psi_n_t <- function(n, tx, tol, verbose) {

  .C("psi_n_t_c",
     as.integer(n),
     as.double(tx),
     as.double(tol),
     as.integer(verbose),
     out = double(1))$out

} # psi_n_t


## TODO:  remove the checkargs argument--and if the calling frame of dkbinom is pkbinom, then
## don't run the checks

##' Probability functions for the sum of k independent binomials
##'
##' The mass and distribution functions of the sum of k independent binomial
##' random variables, with possibly different probabilities.
##'
##' \code{size[1]} and \code{prob[1]} are the size and probability of the first
##' binomial variate, \code{size[2]} and \code{prob[2]} are the size and
##' probability of the second binomial variate, etc.
##'
##' If the elements of \code{prob} are all the same, then \code{pbinom} or
##' \code{dbinom} is used.  Otherwise, repeating convolutions of the k
##' binomials are used to calculate the mass or the distribution functions.
##'
##' NOTE: When \code{log.p} or \code{log} is \code{TRUE}, these functions do
##' not have the same precision as \code{dbinom} or \code{pbinom} when the
##' probabilities are very small, i.e, the values tend to go to \code{-Inf}
##' more quickly.
##'
##' @export dkbinom pkbinom
##'
##' @aliases dkbinom pkbinom
##'
##' @rdname dkbinom
##'
##' @usage
##' dkbinom(x, size, prob, log = FALSE, verbose = FALSE,
##'         method = c("butler", "fft"), checkArgs = TRUE)
##' pkbinom(q, size, prob, log.p = FALSE, verbose = FALSE,
##'         method = c("butler", "naive", "fft"))
##' 
##' @param x Vector of values at which to evaluate the mass function of the sum
##' of the k binomial variates
##'
##' @param q Vector of quantiles (value at which to evaluate the distribution
##' function) of the sum of the k binomial variates
##' 
##' @param size Vector of the number of trials
##' 
##' @param prob Vector of the probabilities of success
##' 
##' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
##' (See NOTE in details).
##' 
##' @param verbose \code{= TRUE} produces output that shows the iterations of
##' the convolutions and 3 arrays, A, B, and C that are used to convolve and
##' reconvolve the distributions.  Array C is the final result.  See the source
##' code in \code{dkbinom.c} for more details.
##' 
##' @param method The \code{butler} (default) method is the
##' algorithm given by Butler, et al. The \code{naive} method is an alternative approach
##' that can be  much slower that can handle no more the sum of five binomials, but
##' is useful for validating the other methods. The \code{naive} method only works
##' for a single value of \code{q}. The \code{fft} method (default) uses the fast Fourier
##' transform to compuation of the convolution of k binomial random variates.
##'
##' @param checkArgs \code{=TRUE} checks the incoming arguments \code{x}, \code{size},
##' \code{prob} for correct structure and consistency.
##' 
##' @return \code{dkbinom} gives the mass function, \code{pkbinom} gives the
##' distribution function.  Produces errors for invalid inputs of \code{size},
##' \code{prob}, \code{x}, and \code{q}.
##' 
##' @author Landon Sego and Alex Venzin
##' 
##' @seealso \code{\link{dbinom}}, \code{\link{pbinom}}
##' 
##' @references The Butler method is based on the exact algorithm discussed by
##' Butler, Ken and Stephens, Michael. (1993) The Distribution of a Sum of
##' Binomial Random Variables. Technical Report No. 467, Department of
##' Statistics, Stanford University.
##' 
##' @keywords misc
##' 
##' @examples
##'# A sum of 3 binomials...
##'dkbinom(c(0, 4, 7), c(3, 4, 2), c(0.3, 0.5, 0.8))
##'dkbinom(c(0, 4, 7), c(3, 4, 2), c(0.3, 0.5, 0.8), method = "b")
##'pkbinom(c(0, 4, 7), c(3, 4, 2), c(0.3, 0.5, 0.8))
##'pkbinom(c(0, 4, 7), c(3, 4, 2), c(0.3, 0.5, 0.8), method = "b")
##'
##'# Compare the output of the 3 methods
##'pkbinom(4, c(3, 4, 2), c(0.3, 0.5, 0.8), method = "fft")
##'pkbinom(4, c(3, 4, 2), c(0.3, 0.5, 0.8), method = "butler")
##'pkbinom(4, c(3, 4, 2), c(0.3, 0.5, 0.8), method = "naive")
##'
##'# Some inputs
##'n <- c(30000, 40000, 20000)
##'p <- c(0.02, 0.01, 0.005)
##'
##'# Compare timings
##'x1 <- timeIt(pkbinom(1100, n, p, method = "butler"))
##'x2 <- timeIt(pkbinom(1100, n, p, method = "naive"))
##'x3 <- timeIt(pkbinom(1100, n, p, method = "fft"))
##'pvar(x1, x1 - x2, x2 - x3, x1 - x3, digits = 12)

# The mass function
dkbinom <- function(x, size, prob, log = FALSE, verbose = FALSE,
                    method = c("butler", "fft"), checkArgs = TRUE) {

## TODO:  remove the checkargs argument--and if the calling frame of dkbinom is pkbinom, then
## don't run the checks, since pkbinom already runs the checks
  if (checkArgs) {
    x <- check.kbinom(x, size, prob)
  }

  # If only one type of variate was requested or if the probs are equal:
  # If the probs are all equal
  if (all(diff(prob) == 0) | (length(prob) == 1)) {
    return(dbinom(x, sum(size), prob[1], log = log))
  }
  
  method <- match.arg(method)
  
  if (method == "fft") {
    
    dkb <- function(x, size, prob) {
    
      A <- dbinom(0:x, size[1], prob[1])
      
      B <- dbinom(0:x, size[2], prob[2])
      
      conv <- cvolve(A, B)
      
      if (length(size) > 2) {
        
        for(i in 3:length(size)){
          
          A <- conv 
          
          B <- dbinom(0:x, size[i], prob[i])
          
          conv <- cvolve(A,B)
          
        }
  
        res <- conv[length(conv)]
        
      } else {
        
        res <- conv[length(conv)]
        
      } # if length...
      
      # fft can produce results < 0 when value is 
      # very close to zero
      res <- abs(res)

      return(res)
    
    } # dkb
    
    # So that I can call it for length(q) > 1
    res <- unlist(lapply(x, function(q) dkb(q, size, prob)))

  # Butler method
  } else {

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
              double(1))[["out"]][x+1]
    
  } # else

  if (log) {
    res <- log(res)
  }

  return(res)

} # dkbinom

# The distribution function
pkbinom <- function(q, size, prob, log.p = FALSE, verbose = FALSE, 
                    method = c("butler", "naive", "fft")) {

  x <- check.kbinom(q, size, prob)

  # If only one type of variate was requested or if the probs are equal:
  # If the probs are all equal
  if (all(diff(prob) == 0) | (length(prob) == 1)) {
    return(pbinom(x, sum(size), prob[1], log.p = log.p))
  }

  method <- match.arg(method)

  # FFT method
  if (method == "fft") {
    
    res <- unlist(lapply(q, function(k) {
      
      sum(unlist(lapply(0:k,
                        function(x) dkbinom(x, size, prob, method = "fft",
                                            checkArgs = FALSE))))
      
    }))

  # Butler method
  } else if ((method == "butler") | (length(size) > 5)) {
  
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
              double(1))[["out"]]
  
    res <- cumsum(res)[x+1]

  # Naive method
  }  else {

   if (length(x) > 1) {
    x <- x[1]
    warning("The 'naive' method is only implemented for a single value of 'q'\n",
            "Only the first value of q[1] = ", q[1], " has been used.")
   }
       
      
   res <- .C("pkbinom",
             as.integer(c(rep(0, 5 - length(size)), size)),
             as.double(c(rep(1, 5 - length(prob)), prob)),
             as.integer(x),
             out = double(1))[["out"]]
             
  } # else Naive
  

  if (log.p) {
    res <- log(res)
  }

  return(res)

} # pkbinom

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


# R has a convolve function, but it's not what we want.
cvolve <- function(x, y) {
  
  preLength <- length(x)
  
  n <- length(x) + length(y) - 1
  
  x <- c(x, rep(0, n - length(x)))
  
  y <- c(y, rep(0, n - length(y)))
  
  out <- Re(fft(fft(x) * fft(y), inverse = TRUE)) / n
  
  return(out[1:preLength])
  
} # cvolve



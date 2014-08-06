# Sum of 2 Binomial RV's
# X ~ Bin(n,p)
# Y ~ Bin(m,q)
# Z = X + Y
# f_Z(z) = sum{i=0,z} f_X(i)f_Y(z-i)

# Landon Sego,  2008-03-05

# Mass function of Z
d2binom <- function(x, size1, prob1, size2, prob2, log=FALSE) {

  # Verify the lengths of inputs are all 1
  for (v in c("x","size1","prob1","size2","prob2","log")) {
    if (length(vg <- get(v)) > 1) {
      assign(v, vg[1])
      warning(v, " has length > 1.  Will only use the first element of ", v, ".\n")
    }
  }

  # Check inputs  
  if ((prob1 < 0) |
      (prob1 > 1) |
      (prob2 < 0) |
      (prob2 > 1) |
      (size1 < 0) |
      (size2 < 0)) {
    warning("NaNs produced\n")
    return(NaN)
  }

  # Verify x is an integer
  if (x %% 1) {
    warning("non-integer x =", x, " will be set to ", round(x), "\n")
    x <- round(x)
  }

  # quick results
  if ((x > size1 + size2) | (x < 0))
    out <- 0

  # If one or both of the sizes are 0 (degenerate cases)
  else if ((s1.0 <- size1 == 0) | (s2.0 <- size2 == 0)) {
    if (s1.0 & s2.0) 
      out <- as.numeric(x == 0)
    else if (s1.0)
      return(dbinom(x, size2, prob2, log=log))
    else 
      return(dbinom(x, size1, prob1, log=log))
  }
  
  
  # If probabilities are equal
  else if (prob1 == prob2)
    return(dbinom(x, size1 + size2, prob1, log=log))

  # Otherwise use the convolution
  else
    out <- .C("d2binom",
              as.integer(x),
              as.integer(size1),
              as.double(prob1),
              as.integer(size2),
              as.double(prob2),
              fZ = double(1),
              PACKAGE="pnlStat")$fZ

  if (log)
    out <- log(out)
  
  return(out)

} # d2binom


# Distribution function of Z
p2binom <- function(q, size1, prob1, size2, prob2, lower.tail=TRUE, log.p=FALSE) {

  # Verify the lengths of inputs are all 1
  for (v in c("q","size1","prob1","size2","prob2","lower.tail","log.p")) {
    if (length(vg <- get(v)) > 1) {
      assign(v, vg[1])
      warning(v, " has length > 1.  Will only use the first element of ", v, ".\n")
    }
  }
  
  # Check inputs  
  if ((prob1 < 0) |
      (prob1 > 1) |
      (prob2 < 0) |
      (prob2 > 1) |
      (size1 < 0) |
      (size2 < 0)) {
    warning("NaNs produced\n")
    return(NaN)
  }

  # Convert q to an integer (always rounds down) if necessary
  q <- floor(q)

  # Simple outcomes...
  if (q >= size1 + size2)
    out <- 1

  else if (q < 0)
    out <- 0

  # If one or both of the sizes are 0 (degenerate cases)
  else if ((s1.0 <- size1 == 0) | (s2.0 <- size2 == 0)) {
    if (s1.0 & s2.0)
      out <- 1 # This works since the condition that q < 0 has already been covered
    else if (s1.0)
      return(pbinom(q, size2, prob2, lower.tail=lower.tail, log.p=log.p))
    else 
      return(pbinom(q, size1, prob1, lower.tail=lower.tail, log.p=log.p))
  }

  # If probabilites are equal
  else if (prob1 == prob2)
    return(pbinom(q, size1 + size2, prob1, lower.tail=lower.tail, log.p=log.p))

  # Otherwise use the convolution
  else
    out <- .C("p2binom",
              as.integer(q),
              as.integer(size1),
              as.double(prob1),
              as.integer(size2),
              as.double(prob2),
              pZ=double(1),
              PACKAGE="pnlStat")$pZ

  if (!lower.tail)
    out <- 1 - out

  if (log.p)
    out <- log(out)
  
  return(out)
  
} # p2binom

##########################################################################################
## QA of code
##########################################################################################

## Result of tests:
## Everything looks good--except the 'log' option is not very robust for very small probabilities and is
## not a precise as the algorithms used by binom and pbinom...


# A quick check--looks good
#X <- rbinom(10^7, 10, 0.6)
#Y <- rbinom(10^7, 20, 0.8)
#Z <- X + Y

#emp <- table(Z) / 10^7
#theo <- double(31)

#for (i in 0:30) 
#  theo[i+1] <- dZ(i, 10, 0.6, 20, 0.8)

#names(theo) <- 0:30

# Let's do a check using same 'p' for both binomials
# (with the appropriate 'else condition' commented out...)

# Testing the density function
#testd <- function(size1, size2, prob, ulimit=NULL, ...) {

#  if (!is.null(ulimit))
#    range.values <- -1:ulimit
#  else
#    range.values <- -1:(size1+size2+1)
  
#  Rvalue <- dbinom(range.values, size1+size2, prob, ...)
#  mValue <- double(length(range.values))
#  for (i in 1:length(range.values))
#    mValue[i] <- d2binom(range.values[i], size1, prob, size2, prob, ...)

#  rMat <- matrix(Rvalue, ncol=1, dimnames=list(range.values, "dProb"))
#  mMat <- matrix(mValue, ncol=1, dimnames=list(range.values, "dProb"))

#  out <- dframeEquiv(rMat, mMat, maxAbsError=1e-12, maxRelError=1e-12)

#  invisible(list(comp=out, range.values=range.values, Rvalue=Rvalue, mValue=mValue))

#}


#timeIt(testd(300,532, 0.2))
#timeIt(testd(300,532, 0.2, ulimit=670, log=TRUE))  # above this the log of my function doesn't have the resolution and it goes to -Inf
#timeIt(testd(200,0,0.1))


#testp <- function(size1, size2, prob, ulimit=NULL, ...) {

#  if (!is.null(ulimit))
#    range.values <- -1:ulimit
#  else
#    range.values <- -1:(size1+size2+1)
  
#  Rvalue <- pbinom(range.values, size1+size2, prob, ...)
#  mValue <- double(length(range.values))
#  for (i in 1:length(range.values))
#    mValue[i] <- p2binom(range.values[i], size1, prob, size2, prob, ...)

#  rMat <- matrix(Rvalue, ncol=1, dimnames=list(range.values, "dProb"))
#  mMat <- matrix(mValue, ncol=1, dimnames=list(range.values, "dProb"))

#  out <- dframeEquiv(rMat, mMat, maxAbsError=1e-12, maxRelError=1e-12)

#  invisible(list(comp=out, range.values=range.values, Rvalue=Rvalue, mValue=mValue))

#}


#timeIt(testp(30,53, 0.92))
#All elements of 'rMat' and 'mMat' appear to be equivalent.
#4.02 minutes 

#timeIt(testp(20,38,0.3,log.p=TRUE))
#All elements of 'rMat' and 'mMat' appear to be equivalent.
#0.1 seconds

# testp(20, 38, 0.3, ulimit=33, log.p=TRUE, lower.tail=FALSE)  ## note the limit required to acheive equivalence
# All elements of 'rMat' and 'mMat' appear to be equivalent.

#timeIt(testp(20, 38, 0.3, lower.tail=FALSE))
#All elements of 'rMat' and 'mMat' appear to be equivalent.
#0.09 seconds 

#Timeit(Testp(0, 90, 0.7))
#All Elements Of 'Rmat' And 'Mmat' Appear To Be Equivalent.
#0.26 Seconds 

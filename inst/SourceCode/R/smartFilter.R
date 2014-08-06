# Calculates a moving average for a timeseries, but it includes the endpoints
# similar to 'filter'

# Landon Sego  April 2008

smartFilter <- function(y, weights, min.window=1, start=1, skip=1, balance=TRUE) {

  # weights should not be missing
  if (any(is.na(weights)))
    stop("The elements of 'weights' can not be NA\n")

  # Weights should not sum to 0
  if (abs(sw <- sum(weights)) < 1e-10)
    stop("'weights' must not sum to 0\n")
  
  # weights should sum to 1
  if (abs(sw - 1) > 1e-10) {
    weights <- weights / sw
    warning("Weights were rescaled to sum to 1.\n")
  }

  # Define the window bandwith (in terms of the number of data points)
  bw <- length(weights) %/% 2

  if (!(length(weights) %% 2))
    stop("Length of 'weights' must be odd\n")

  n <- length(y) 

  # More checks
  if (start > n)
    stop("'start' must be <= 'length(y)'\n")
  if (skip <= 0)
    stop("'skip' must be > 0\n")
  if (start > bw + 1)
    warning("Since 'start' > (length(weights) %/% 2), part of the data at the beginning\n",
            "  of the series will not be covered by a window\n")
  if (skip > 2 * bw + 1)
    warning("Since 'skip' > the window length, parts of the data throughout\n",
            "  the series will not be covered by a window\n")
  
  # Index of window centers
  win.centers <- seq(start, n, by=skip)
  num.windows <- length(win.centers)

  # Calculate the moving dot products
  out <- .C("smartFilter",
            as.double(y),
            as.integer(!is.na(y)),
            as.integer(n),
            as.double(weights),
            as.integer(bw),
            as.integer(min.window),
            as.integer(win.centers - 1),
            as.integer(num.windows),
            as.integer(balance),
            out = double(num.windows),
            NAOK = TRUE,
            PACKAGE = "pnlStat")$out

  # Give appropriate names to out
  if (!is.null(ny <- names(y)))
    names(out) <- ny[win.centers]

  # List the vector with the moving averages
  out
  
} # smartFilter




# A function to test/validate the smartFilter which uses a slower algorithm.  The tests showed equivalence...

#smartFilterTest <- function(y, weights, min.window=1, start=1, skip=1, balance=TRUE) {

#  # weights should not be missing
#  if (any(is.na(weights)))
#    stop("The elements of 'weights' can not be NA\n")

#  # Weights should not sum to 0
#  if (abs(sw <- sum(weights)) < 1e-10)
#    stop("'weights' must not sum to 0\n")
  
#  # weights should sum to 1
#  if (abs(sw - 1) > 1e-10) {
#    weights <- weights / sw
#    warning("Weights were rescaled to sum to 1.\n")
#  }

#  # Define the window bandwith (in terms of the number of data points)
#  bw <- length(weights) %/% 2

#  if (!(length(weights) %% 2))
#    stop("Length of 'weights' must be odd\n")

#  n <- length(y) 

#  # More checks
#  if (start > n)
#    stop("'start' must be <= 'length(y)'\n")
#  if (skip <= 0)
#    stop("'skip' must be > 0\n")
#  if (start > bw + 1)
#    warning("Since 'start' > (bandwidth + 1), part of the data at the beginning\n",
#            "  of the series will not be covered by a window\n")
#  if (skip > 2 * bw + 1)
#    warning("Since 'skip' > the window length, parts of the data throughout\n",
#            "  the series will not be covered by a window\n")
  
#  # Index of window centers
#  win.centers <- seq(start, n, by=skip)
#  num.windows <- length(win.centers)

#  # Make the response matrix and select the windows that will be calculated
#  if (!require(calcIVT))
#    stop("package 'calcIVT' is required for smartFilterTest\n")

#  mat <- make.response.mat(y, bw=bw)[,win.centers]

#  # Now apply the weights in the "smart way" over the windows
#  dprod <- function(x) {

#    if (all(not.na <- !is.na(x)))
#      return(as.numeric(t(weights) %*% x))
#    else {

#      calc.dp <- TRUE
      
#      # 1. Num of nonNA is at least as big as the min window size      
#      if (sum(not.na) >= min.window) {
        
#        if (balance) {
#          first.non.missing <- which(not.na)[1]
#          last.non.missing <- which(rev(not.na))[1]
#          last.non.missing <- c(length(weights):1)[last.non.missing]
          
#          # 2. First nonNA occurs on or before center point of window
#          # 3. Last nonNA occurs on or after center point of window
#          if (!((first.non.missing <= bw + 1) & (last.non.missing  >= bw + 1)))
#            calc.dp <- FALSE
#        }
#      }
#      else
#        calc.dp <- FALSE

#      if (calc.dp) {
#        new.wts <- weights[not.na]
#        new.wts <- new.wts/sum(new.wts)
#        return(as.numeric(t(x[not.na]) %*% new.wts))
#      }
#      else
#        return(NA)
      
#    } # else there are missing

#  } # dprod()

#  # Calculate the weighted averages
#  out <- apply(mat, 2, dprod)

#  # Give appropriate names to out
#  if (!is.null(ny <- names(y)))
#    names(out) <- ny[win.centers]

#  # List the vector with the moving averages
#  out
  
#} # smartFilterTest



## Testing
#test <- function() {

#  for (len in c(500,1000)) {
#    for (m.pct in c(0,0.10,0.20,0.70,0.90)) {
#      for (wts in list(rep(1,31)/31, (1:11)/66)) {
#        for (skip in c(1,2,7)) {
#          for (min.win in c(1,3,5)) {
#            for (start in c(1, 7)) {
#              for (bal in c(TRUE,FALSE)) {
#                yvec <- rnorm(len)
#                yvec[sort(sample(1:len, floor(m.pct*len)))] <- NA
#                t1 <- data.frame(out=smartFilter(yvec, wts, min.window=min.win, start=start, skip=skip, balance=bal))
#                t2 <- data.frame(out=smartFilterTest(yvec, wts, min.window=min.win, start=start, skip=skip, balance=bal))
#                if (!dframeEquiv(t1,t2, maxAbsError=1e-12,maxRelError=1e-12, verbose=FALSE)$equiv) {
#                  cat("This not equiv:\n")
#                  pvar(len,m.pct,wts,skip,min.win,start,bal)
#                }
#              }
#            }
#          }
#        }
#      }
#      cat("Status:  Just finished\n")
#      pvar(len,m.pct)
#    }
#  }
  

#} # test



#> test()
#Status:  Just finished
#len = 500, m.pct = 0 
#Status:  Just finished
#len = 500, m.pct = 0.1 
#Status:  Just finished
#len = 500, m.pct = 0.2 
#Status:  Just finished
#len = 500, m.pct = 0.7 
#Status:  Just finished
#len = 500, m.pct = 0.9 
#Status:  Just finished
#len = 1000, m.pct = 0 
#Status:  Just finished
#len = 1000, m.pct = 0.1 
#Status:  Just finished
#len = 1000, m.pct = 0.2 
#Status:  Just finished
#len = 1000, m.pct = 0.7 
#Status:  Just finished
#len = 1000, m.pct = 0.9 

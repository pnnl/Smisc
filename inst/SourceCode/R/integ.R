# Uses Simpson's rule to approximate an integral.

# Ellis R, Gulick D. "Calculus: One and Several Variables,"
# Harcourt Brace Jovanovich, Publishers: New York, NY, 1991; 479-482.

integ <- function(y, x = NULL, a = NULL, b = NULL, method = c("simpson", "trapezoid")) {

  # Match args of the methos
  method <- match.arg(method)

  if (method == "simpson") {

    if (!is.null(x)) 
      warning("'x' is not use in Simpson's method")

    if (is.null(a) | is.null(b))
      stop("Both endpoints 'a' and 'b' are required for Simpson's method")
  
    # y is a vector of the values of the function, evaluated at an
    # odd number of evenly spaced points in the interval [a,b].
    # a is lower limit, b is upper limit
  
    # Checks
    if (a > b)
      stop("a must be less than or equal to b.\n")
  
    if ((length(y)+1)%%2)
      stop("length(y) must be odd.\n")
  
    if (length(y) < 3)
      stop("length(y) must be an odd number >= 3.\n")
  
    # Degenerate integral
    if (a == b)
      return(0)
    
    # Calculate the number of intervals
    n <- length(y) - 1
            
    # Example of the 'Simpson' weights:
    #
    # If the length of y was 11, the wts vector would be:
    # 1 4 2 4 2 4 2 4 2 4 1
    #
    # If the length of y was 5, the wts vector would be:
    # 1 4 2 4 1
    #
    # If the length of y was 3, the wts vector would be:
    # 1 4 1
    
    # Generate the Simpson weights
    #  wts <- double(length(y))
    #  for (i in 1:length(y)) {
    #    if (i%%2) wts[i] <- 2
    #    else wts[i] <- 4
    #  }
  
    # We can generate the weights without the loop
    wts <- ifelse((1:length(y))%%2, 2, 4)
  
    # The first and the last weights are 1
    wts[1] <- wts[length(wts)] <- 1
  
    # Return the approximation of the integral
    return(as.vector( ((b-a)/(3*n)) * (t(y) %*% wts) ))
    
  } # Simpson's method

  # Trapezoid method
  else {

    # Checks
    if (is.null(x))
      stop("'x' is required for the trapezoid method")

    if (!(is.null(a) & is.null(b)))
      warning("Neither 'a' nor 'b' are used with the trapezoid rule")

    if (length(x) != length(y))
      stop("Length's of 'x' and 'y' must be the same")

    if (length(y) < 2)
      stop("'y' must have at least length 2")

    if (!all(diff(x) > 0))
      stop("'x' must be sorted in ascending order and not have duplicate values")

    # Algorithm from http://rgm2.lab.nig.ac.jp/RGM2/func.php?rd_id=caTools:trapz
    
    idx <- 2:length(x)
    
    return(as.double((x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1]) / 2))

  } # trapezoid method


} # end integ()

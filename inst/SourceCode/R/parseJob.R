parseJob <- function(n, njobs, collate = FALSE, random.seed = NULL, text.to.eval = FALSE) {

  # Sanity checks
  if ((n <= 0) | (njobs <= 0))
    stop("'n' must be greater than or equal to 'njobs', and both must be positive\n")

  # If njobs is too large
  if (n < njobs) {
    warning("'n' must be greater than or equal to 'njobs'\n",
            "Setting 'njobs' to ", n)
    njobs <- n
  }

  if (text.to.eval & !is.null(random.seed)) {
    warning("No randomization is performed when 'text.to.eval = TRUE'")
    random.seed <- NULL
  }

  # If we want to process the elements in sequence
  if (collate) {

    if (!is.null(random.seed)) {
      warning("No randomization is performed when 'collate = TRUE'")
      random.seed <- NULL
    }

    out <- vector(mode = "list", length = njobs)

    for (i in 1:njobs)
      out[[i]] <- paste("seq(", i, ", ", n, ", by = ", njobs, ")", sep = "")
    
  } # if collating

  else {

    # Use integer division and the mod to count how many need to be in each job
    n.per.job <- rep(n %/% njobs, njobs)
    n.left.over <- n %% njobs
      
    # Check 1
    if (n != (n.per.job[1] * njobs + n.left.over))
      stop("Algorithm failure 1")
  
    # Add 1 to the first elements of n.per.job
    if (n.left.over)
      n.per.job[1:n.left.over] <- n.per.job[1:n.left.over] + 1
  
    # Check 2
    if (sum(n.per.job) != n)
      stop("Algorithm failure 2")

    if (is.null(random.seed)) {
      
      hi <- cumsum(n.per.job)
      lo <- hi - n.per.job + 1
      out <- as.list(paste(lo, hi, sep = ":"))

    }
  
    else {  
      
      # Define the cases
      cases <- 1:n
    
      # Set the seed and randomize the cases
      set.seed(random.seed)
      cases <- sample(cases)
    
      # The list building function
      subEnv <- new.env()
      assign("counter", 0, envir = subEnv)
    
      # Function that will build the vectors that show the parsing
      lb <- function(x) {
        counter.local <- get("counter", envir = subEnv)
        assign("counter", max(x) + counter.local, envir = subEnv)
        return(cases[counter.local + 1:x])
      }
      
      # Build the output list
      out <- lapply(as.list(n.per.job), lb)
    
      # Check 3
      check.out <- unlist(out)
      if (!is.null(random.seed))
        check.out <- sort(check.out)
    
      if (!all(check.out == 1:n))
        stop("Algorithm failure 3")

    } # else randomization
  
  } # else no collating

  # Convert from text if text.to.eval == FALSE
  if (!text.to.eval & is.null(random.seed))
    out <- lapply(out, function(x) eval(parse(text = x)))
  
  # Verify that the length of the list is equal to njobs
  if (length(out) != njobs)
    stop("Algorithm failure 4")

  return(out)
  
} # parseJob

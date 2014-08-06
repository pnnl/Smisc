checkBatch <- function(file, check.interval.sec = 2, max.hours = 24, wait = TRUE) {

 # Check the platform
  os <- .Platform$OS.type
  if (!(os %in% c("unix", "windows")))
    stop("checkBatch() is currently only supported on the Unix and Windows platforms\n")
  os.win <- os == "windows"
    
  c1 <- c2 <- 0

  elapsed.hours <- 0
  start.time <- Sys.time()

  # Verify the file exists
  if (!file.exists(file))
    stop("'", file, "' does not exist\n")
  
  # Search for file
  while ((c1 == 0) & (c2 == 0) & (elapsed.hours < max.hours)) {

    if (os.win) {
      orig.opt <- options(warn = -1)
      c1 <- length(shell(paste('findstr /M "proc.time()" ', file, sep=""), intern=TRUE))
      c2 <- length(shell(paste('findstr /M "Execution halted" ', file, sep=""), intern=TRUE))
      c3 <- length(shell(paste('findstr /M "Error" ', file, sep=""), intern=TRUE))
      options(orig.opt)
    }
    else {
      c1 <- as.numeric(system(paste("tail -3 ", file, " | grep 'proc.time()' | wc -l", sep=""), intern=TRUE))
      c2 <- as.numeric(system(paste("tail -3 ", file, " | grep 'Execution halted' | wc -l", sep=""), intern=TRUE))
      c3 <- as.numeric(system(paste("tail -3 ", file, " | grep 'Error' | wc -l", sep=""), intern=TRUE))
    }

    # pvar(c1,c2)

    # If we're not waiting, then break the while loop
    if (!wait)
      break

    if ((c1 == 0) & (c2 == 0) & (c3 == 0)) 
      Sys.sleep(check.interval.sec)

    elapsed.hours <- as.numeric(difftime(Sys.time(), start.time, units="h"))

  }
    
  if ((c1 == 1) & (c2 == 0) & (c3 == 0))
    return("success")
  else if ((c1 == 0) & ((c2 == 1) | (c3 > 0)))
    return("fail")
  else if ((c1 == 0) & (c2 == 0) & (c3 == 0))
    return("incomplete")
  else
    stop("Unexpected outcome:", pvar(c1, c2, c3, verbose=FALSE))
    
} # checkBatch
  

plapply <- function(X, FUN, ...,
                    packages = NULL,
                    header.file = NULL,
                    needed.objects = NULL,
                    needed.objects.env = parent.frame(),
                    jobName = "plapply",
                    njobs = 7,
                    max.hours = 24,
                    check.interval.sec = 30,
                    collate = FALSE,
                    random.seed = NULL,
                    clean.up = TRUE,
                    rout = !clean.up,
                    verbose = FALSE) {

  # Header file is a file containing R code that will be sourced which create an 'environment'
  # that will satisfy all potential dependencies that FUN or X may have.
  # ..., objects that will be saved to the parallel environment on which FUN may depend
  #       (not additional arguments to FUN)

  # Check the platform
  os <- .Platform$OS.type
  if (!(os %in% c("unix", "windows")))
    stop("plapply() is currently only supported on the Unix and Windows platforms\n")
  os.win <- os == "windows"

  # Check the needed.objects for reserved names
  if (!is.null(needed.objects)) {

    # Make sure 'needed.objects' does not include the following
    reserved.global.objects <- c("X.i", "FUN.p", "FUN.argnames", "optional.args", "process.id")

    conflict.objects <- reserved.global.objects[reserved.global.objects %in% needed.objects]
    
    if (length(conflict.objects))
      stop("The following are reserved objects for 'plapply', and should not be included ",
           "in the 'needed.objects' argument:\n '", paste(conflict.objects, collapse = "', '"), "'\n")
  }

  # Set njobs to an integer value
  njobs <- as.integer(njobs)

  # If njobs is negative or non-integer
  if (njobs < 1)
    stop("'njobs' must be >= 1")

  ################################################################################
  # Run regular lapply if only one job requested
  ################################################################################
  if (njobs == 1) {

    # Load packages
    if (!is.null(packages)) {
      for (pk in packages)
        require(pk, character.only = TRUE)
    }

    # Source the header file
    if (!is.null(header.file))
      source(header.file)

    # Put needed objects into the global environment if they exist
    if (!is.null(needed.objects)) {

      # Verify the parent.env is not the global environment
      if (environmentName(needed.objects.env) != "R_GlobalEnv") {
        
        # Send the objects to the global environment
        for (v in needed.objects) {
          
          if (exists(v, where = .GlobalEnv, inherits = FALSE))
            warning("'", v, "' existed in the global environment and was replaced by the object\n",
                    "of the same name in the environment specified by the 'needed.objects.env' argument.")
          
          assign(v, get(v, pos = needed.objects.env), pos = .GlobalEnv)
        }

      } # not global
      
    } # getting needed objects needed

    # Run and return the lapply and return the results
    out <- lapply(X, FUN, ...)

    # Remove objects from global environment if they were stored there
    if ((!is.null(needed.objects)) & (environmentName(needed.objects.env) == "R_GlobalEnv")) 
      rm(list = needed.objects, pos = .GlobalEnv)

    # Return the list and exit the function
    return(out)
    
  } # njobs == 1

  
  # Calculate the list length
  lenX <- length(X)
  
  if (lenX < njobs) {
    warning("Number of requested job exceeds the number of elements in the list.\n",
            "'njobs' will be reduced to", lenX)
    njobs <- lenX
  }

  # To ensure this is unique
  # jobName + time stamp + proc.time + 15 random digits
  rsf <- paste(stripExtension(timeStamp(paste(jobName, sep = "_"), "nothing")),
               round(proc.time()[3] * 1000),
               round(runif(1) * 10^15), sep = "_")

  # Files that will be deleted
  tmp.files <- f.R <- list.outputs <- NULL

  # Match functions
  FUN <- match.fun(FUN)

  # Get the optional args to FUN into a list
  optional.args <- list(...)

  # Now add needed.objects into this environment so they can be saved from here, along with other objects
  # in this environment.  Add suffix to their names so they will be unique from any other objects
  # in this environment
  if (!is.null(needed.objects)) {

    # Add suffix to the names
    for (v in needed.objects)
      assign(paste(v, "neededObjects", sep = "."), get(v, pos = needed.objects.env))

    needed.objects <- paste(needed.objects, "neededObjects", sep = ".")

  }

  # Match the arguments that actually exist in FUN
  FUN.argnames <- names(formals(FUN))

  if (length(optional.args)) {

    # If any optional arguments are provided that are not in FUN
    if (!all(names(optional.args) %in% FUN.argnames[-1])) 
      stop("Optional arguments do not match the optional arguments of FUN")
          
    FUN.p <- function(x) {
      xList <- eval(parse(text = paste("list(", FUN.argnames[1], "= x)", sep = "")))
      do.call("FUN", c(xList, optional.args))
    }
    
  }
  else
    FUN.p <- FUN

  
  # Identify how to parse the job
  subsets <- parseJob(lenX, njobs, collate = collate, random.seed = random.seed)

  # Prepare text strings that will be written to the .R files that are separately launched
  
  # Load packages as requested
  if (is.null(packages))
    pk.text <- "# No packages needed\n"
  else {
    pk.text <- NULL
    for (pk in packages)
      pk.text <- c(pk.text, paste("require(", pk, ")\n", sep = ""))
  }
  
  # Prepare the header file
  if (!is.null(header.file))
    hf.text <- paste("source('", header.file, "')\n", sep = "")
  else
    hf.text <- "# No header file included.\n"

 # Rename needed objects if they are present (strip the suffix)
  if (!is.null(needed.objects))  
    mv.text <- c("neededObjects.vec <- ls(pattern = 'neededObjects')\n",
                 "for (v in neededObjects.vec)\n",
                 "  assign(stripExtension(v), get(v))\n",
                 "rm(list = neededObjects.vec, neededObjects.vec)\n")
  else
    mv.text <- "# No needed.objects to rename.\n"

  # Run the separates instances of lapply
  for (i in 1:njobs) {

    # File name for the R code that will be called by launching a separate instance of R
    fname <- paste(rsf, "_", i, ".R", sep = "")

    # Collecting the R code filenames
    f.R <- c(f.R, fname)

    # The subset of the list that will be operated on
    subset <- subsets[[i]]

    # suffixes for filenames which identify the subset
    suffix <- paste(padZero(min(subset), nchar(as.character(lenX))), "-",
                    padZero(max(subset), nchar(as.character(lenX))), ".Rdata", sep = "")
    
    # The filename for all the input to the lapply
    list.input.i <- paste(rsf, "inputData", suffix, sep = "_")

    # The filename for the output from the lapply
    list.output.i <- paste(rsf, "outputData", suffix, sep = "_")

    # Concatentate output filenames for use in collecting the results later
    list.outputs <- c(list.outputs, list.output.i)
    tmp.files <- c(tmp.files, list.input.i, list.output.i)

    # Save the environment that will be needed
    X.i <- X[subset]
    save(list = needed.objects, X.i, FUN.p, FUN.argnames, optional.args, file = list.input.i)
    
    # Write the .R file that will be launched
    cat("require(pnlStat)\n",
        
        # Load packages
        pk.text,
        
        # Source the header file (if there is one)
        hf.text,
        
        # Set the process id variable
        "process.id <- ", i, "\n",
        
        # Load the file that will create the R environment
        "load('", list.input.i, "')\n",
        
        # Rename 'needed.objects' (if they're present)
        mv.text,

        # Evaluate pre-processing expression if it's present
        "if (exists(\"pre.process.expression\"))\n",
        "  eval(pre.process.expression)\n",
        
        # Process this subset of the list
        "X.i.out <- lapply(X.i, FUN.p)\n",

        # Save this output
        "save(X.i.out, file='", list.output.i, "')\n",

        # Evaluate post-processing expression if it's present
        "if (exists(\"post.process.expression\"))\n",
        "  eval(post.process.expression)\n",

        # Print warnings
        "warnings()\n",
        
        sep = "",
        file = fname)

    # Now launch the i_th job, the annoying 'nohup.out' is routed to /dev/null
    if (os.win)
      shell(paste("R CMD BATCH --no-restore --no-save", fname), mustWork = TRUE, wait = FALSE, translate = TRUE)      
    else
      system(paste("nohup R CMD BATCH --no-restore --no-save", fname, "> /dev/null 2>&1 &"))
    
    if (verbose)
      cat("Launching job", i, "\n")

  } # for

  # Wait for the jobs to finish
  njobs.finished <- 0
  elapsed.hours <- 0
  start.time <- Sys.time()

  if (verbose)
    cat("Waiting for jobs to complete...\n")

  while ((njobs.finished < njobs) & (elapsed.hours < max.hours)) {

    Sys.sleep(check.interval.sec)

    if (os.win) {
      orig.opt <- options(warn = -1)
      njf.1 <- length(shell(paste('findstr /M "proc.time()" ', rsf, '_*.Rout', sep = ""), intern = TRUE))
      njf.2 <- length(shell(paste('findstr /M "Execution halted" ', rsf, '_*.Rout', sep = ""), intern = TRUE))
      options(orig.opt)
    }
    else {
      njf.1 <- as.numeric(system(paste("grep -l 'proc.time()' ", rsf, "_*.Rout | wc -l", sep = ""), intern = TRUE))
      njf.2 <- as.numeric(system(paste("grep -l 'Execution halted' ", rsf, "_*.Rout | wc -l", sep = ""), intern = TRUE))
    }
      
    njobs.finished <- njf.1 + njf.2                        
    elapsed.hours <- as.numeric(difftime(Sys.time(), start.time, units="h"))

  }

  # If one of the jobs failed
  if (njf.2)
    stop("It appears that ", njf.2, " of the parallelized jobs failed.\n",
         "Look at the '", rsf, "_*.Rout' files of the separate jobs.\n")
  

  # If they all finished and we didn't time out
  if (njobs.finished == njobs) {

    Xout <- NULL

    # Concatenate the files
    for (f in list.outputs) {
      Xout <- c(Xout, load.object(f))
      if (verbose)
        cat("Adding data from '", f, "'\n", sep = "")
    }

    # If collated or random, need to restore list to original order
    if (collate | !is.null(random.seed))
      Xout <- Xout[order(unlist(subsets))]

    # Some checks
    if (!all(names(Xout) == names(X)))
      warning("!all(names(Xout) == names(X))\n")

    # gather all .Rout files into a single .Rout file
    if (rout) {

      if (os.win)
        shell(paste("type ", rsf, "_*.Rout > ", rsf, ".Rout", sep = ""))
      else
        system(paste("cat ", rsf, "_*.Rout > ", rsf, ".Rout", sep = ""))
      
      if (verbose)
        cat("All ", njobs, " *.Rout files are collected in '", rsf, ".Rout'\n", sep = "")

    }

    
    # Deleting extra, temporary files
    if (clean.up) {

      dfiles <- c(tmp.files, f.R, paste(stripExtension(f.R), "Rout", sep = "."))
      unlink(dfiles)
      
      if (verbose) {
        cat("The following temporary files were deleted:\n")
        print(sort(dfiles))
      }
      
    }
  
    
  } # if (njobs.finished == njobs) {

  else
    stop("Jobs did not complete in the maximum waiting time of ",
         round(max.hours, 5), " hours.",
         "\nOutput files were not concatenated and temprorary files were not deleted.\n")

  return(Xout)

  
} # plapply

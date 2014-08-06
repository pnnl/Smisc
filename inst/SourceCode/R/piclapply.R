# HPC version of lapply, created for PNNL institutional computing cluster olympus.pnl.gov

## Possible TODO: We may not want piclapply to reduce the number of nodes to make sure every core is utilized...
## Having a single node that is underutilized may be prefered...

# Landon Sego

piclapply <- function(X, FUN, account, ...,
                      packages = NULL,
                      header.file = NULL,
                      needed.objects = NULL,
                      needed.objects.env = parent.frame(),
                      jobName = "piclapply",
                      numNodes = 2,
                      partition = c("slurm", "short", "fat", "gpu"),
                      time.limit.mins = 30,
                      check.interval.sec = 30,
                      tarball.file = NULL,
                      use.mclapply = FALSE,
                      mclapply.args = NULL,
                      parseJob.args = NULL, 
                      remove.working.dir = TRUE,
                      email.notification = NULL,                      
                      verbose = FALSE) {
  
  # Print warnings as they occur
  op <- options(warn = 1)  

  ################################################################################
  # Check input arguments
  ################################################################################
  partition <- match.arg(partition)
  
  # Verify these are logicals
  for (v in c("remove.working.dir", "use.mclapply", "verbose")) {
    if (!is.logical(get(v)))
      stop("'", v, "' should be TRUE or FALSE\n")
  }  

  # Get the start time
  if (verbose)
    s.time <- Sys.time()
  
  # Verify we're on olympus or one of its nodes
  usr.host <- system("/bin/hostname -s", intern = TRUE)
  if (!(gsub("[0-9]", "", usr.host) %in% c("olympus", "olympus-e", "node", "gpu", "fat", "short")))
    stop("'piclapply' was written for use on olympus or its computing nodes only\n",
         "Your host name is '", usr.host, "'.")

  # Get the start time
  if (verbose)
    s.time <- Sys.time()
  
  # Verify these arguments are character
  for (v in c("account", "jobName")) {
    if (!is.character(get(v)))
      stop("'", v, "' should be a character string (or vector)")
  }

  # Verify these arguments are character (if they're not NULL)
  for (v in c("packages", "header.file", "needed.objects", "tarball.file", "email.notification")) {
    cv <- get(v)
    if (!is.null(cv)) {
      if (!is.character(cv))
        stop("'", v, "' should be a character string (or vector) or NULL")
    }
  }

  # Verify these are positive integers
  numeric.arguments <- c("numNodes", "time.limit.mins", "check.interval.sec")
                        
  for (v in numeric.arguments) {
    v.bad <- TRUE
    cv <- get(v)
    if (is.numeric(cv)) {
      if ((cv > 0) & (cv %% 1 == 0))
        v.bad <- FALSE
    }
    if (v.bad)
      stop("'", v, "' must be a positive integer")
  }

  # mclapply.args only get used with use.mclapply = TRUE
  if (!is.null(mclapply.args) & (!use.mclapply)) {
    mclapply.args <- NULL
    warning("'mclapply.args' are ignored when 'use.mclapply = FALSE'")
  }
  
  # If the R version is not >= 2.14.0, then use.mclapply must be set to FALSE
  rv <- as.numeric(paste(R.Version()$major, stripExtension(R.Version()$minor), sep = "."))

  if (use.mclapply & (rv < 2.14)) {
    
    use.mclapply <- FALSE
    mclapply.args <- NULL
    
    warning("'use.mclapply = TRUE' can only be used with R 2.14.0 or greater\n",
            "'use.mclapply' has been set to FALSE")
  }

  # Get the length of the list
  lenX <- length(X)

  ################################################################################
  # If the the partition is short, then limitations apply
  ################################################################################
  if (partition == "short") {
    
    if (numNodes > 2) {

      numNodes <- 2

      warning("'numNodes' cannot exceed 2 when using the short partition. ",
              "'numNodes' set to 2.")
    }
    
    if (time.limit.mins > 60) {
      
      time.limit.mins <- 60

      warning("'time.limit.mins' cannot exceed 60 when using the short partition. ",
              "'time.limit.mins' set to 60.")
      
    }
    
  }
  
  ################################################################################
  # Set the number of slurm processes and rescale the job if the list size
  # isn't large enough
  ################################################################################
  
  if (use.mclapply) {

    if (lenX < numNodes) {

      numNodes <- lenX
      
      warning("Job size has been reduced to accomodate the size of the ",
              "list and utilize each\nnode at 100%, if possible:  ",
              pvar(length(X), numNodes, verbose = FALSE))
    }

    num.slurm.processes <- numNodes
    
  }
  else {
    
    # Assign the number of processes
    num.slurm.processes <- 32 * numNodes
  
    # If number of processes exceeds the number elements in the list, then reduce it
    # appropriately
    if (lenX < num.slurm.processes) {
      numNodes <- max(1, lenX %/% 32)
      num.slurm.processes <- ifelse(lenX < 32, lenX, numNodes * 32)
      warning("Job size has been reduced to accomodate the size of the ",
              "list and utilize each\nnode at 100%, if possible:  ",
              pvar(length(X), numNodes, num.slurm.processes, verbose = FALSE))
    }
  }
  
  ################################################################################
  # Implement checks and create working directories & files
  ################################################################################
  
  # Create the working directory
  usrName <- system("whoami", intern = TRUE)
  userScratch <- paste("/pic/scratch", usrName, sep = "/")
  if (!file.exists(userScratch)) 
    system(paste("mkdir", userScratch))

  # Create temporary working directories
  tmpNum <- system("echo $$", intern = TRUE)
  wkdir <- paste(userScratch, "/", jobName, "_tmp_", tmpNum, sep = "")
  wkdir.logs <- paste(wkdir, "logs", sep = "/")
  wkdir.out <- paste(wkdir, "out", sep = "/")
  system(paste("mkdir ", wkdir, "; ",
               "mkdir ", wkdir.logs, "; ",
               "mkdir ", wkdir.out, sep=""))

  # Create temporary working directories and files
  envir.file <- paste(wkdir, "Renvironment.Rdata", sep = "/")
  rscript.file <- paste(wkdir, "pic_lapply.R", sep = "/")
  slurm.batch.script <- paste(wkdir, "/", jobName, ".sh", sep = "")
  sl.out <- paste(wkdir, "slurmLaunch.out", sep = "/")
  sl.err <- paste(wkdir, "slurmLaunch.err", sep = "/")
  sb.err <- paste(wkdir, "sbatch.err", sep = "/")

  # If a tarball.file was not specified
  if (is.null(tarball.file)) 
    tarball.file <- paste(getwd(), "/", jobName, "_output_", tmpNum, ".tar.gz", sep = "")
 
  # If a tarball.file did not have a path (in which case, it should have the path
  # of the working directory
  else if ((tarball.file != "none") & (stripPath(tarball.file) == tarball.file)) 
    tarball.file <- paste(getwd(), tarball.file, sep = "/")
  
  if (verbose) {
    cat("\nCreated working directories and filenames:\n\n")
  
    # Display the various paths and filenames
    pvar(wkdir)
    pvar(wkdir.logs)
    pvar(wkdir.out)
    pvar(envir.file)
    pvar(rscript.file)
    pvar(sl.out)
    pvar(sl.err)
    pvar(sb.err)
    pvar(tarball.file)
        
  }

  ################################################################################
  # Create environmental data file that will be loaded in each process
  ################################################################################
  
  # Match functions
  FUN <- match.fun(FUN)

  # Capture optional arguments to the function
  optional.args <- list(...)

  ################################################################################
  # Identify how to parse the job
  ################################################################################

  # Check the optional args to parseJob
  if (!is.null(parseJob.args)) {

    if (!is.list(parseJob.args))
      stop("'parseJob.args' must be a named list of optional arguments to parseJob")

    # possible optional args to mclapply
    possible.args <- setdiff(names(formals(parseJob)), c("n", "njobs"))

    # Make sure the arguments actually match the optional arguments of parseJob
    if (!all(names(parseJob.args) %in% possible.args))
      stop("The following are not acceptable arguments to 'parseJob':\n",
           "'", paste(setdiff(names(parseJob.args), possible.args), collapse = "', '"), "'")
  }

  # Create the subsets that identify how the list X will be parsed.  Each element of 'subsets'
  # corresponds to a the elements that will be processed by a core (use.mclapply = FALSE) or a node
  # (use.mclapply = TRUE)
  subsets <- do.call(parseJob, c(list(n = lenX, njobs = num.slurm.processes), parseJob.args))


  ################################################################################
  # Now add needed.objects into this environment so they can be saved from here,
  # along with other objects in this environment.  Add suffix to their names so they
  # will be unique from any other objects in this environment
  ################################################################################
  if (!is.null(needed.objects)) {

    # Make sure 'needed.objects' does not include the following
    reserved.global.objects <- c("process.id", "tmpNum", "X", "FUN.p",
                                 "FUN.argnames", "mclapply.optional.args",
                                 "optional.args", "subsets",
                                 "wkdir.out", "num.slurm.processes")

    conflict.objects <- reserved.global.objects[reserved.global.objects %in% needed.objects]
    
    if (length(conflict.objects))
      stop("The following are reserved objects for 'piclapply', and should not be included\n",
           "in the 'needed.objects' argument: '", paste(conflict.objects, collapse = "', '"), "'\n")
    
    for (v in needed.objects)
      assign(paste(v, "neededObjects", tmpNum, sep = "."), get(v, envir = needed.objects.env))

    needed.objects <- paste(needed.objects, "neededObjects", tmpNum, sep = ".")    
    
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

  # Rename the list so that it can be reordered as it was originally
  orig.X.names <- names(X)
  names(X) <- as.character(1:lenX)

  ################################################################################
  # Handle the optional args to mclapply
  ################################################################################
  if (!is.null(mclapply.args)) {

    if (!is.list(mclapply.args))
      stop("'mclapply.args' must be a named list of optional arguments to mclapply")

    # possible optional args to mclapply
    possible.args <- setdiff(names(formals(parallel:::mclapply)), c("X", "FUN", "..."))

    # Make sure the arguments actually match the optional arguments of mclapply
    if (!all(names(mclapply.args) %in% possible.args))
      stop("The following are not acceptable arguments to 'mclapply':\n",
           "'", paste(setdiff(names(mclapply.args), possible.args), collapse = "', '"), "'")

    # If the mc.cores argument was requested
    if ("mc.cores" %in% names(mclapply.args))
      mclapply.optional.args <- mclapply.args
    
    # If mc.cores was not included, add it in
    else
      mclapply.optional.args <- c(mclapply.args, list(mc.cores = 32))
  }

  else
    mclapply.optional.args <- list(mc.cores = 32)
  
  ################################################################################
  # Save the environment that will be needed for each instance of lapply
  ################################################################################
  
  save(list = needed.objects, tmpNum, X, FUN.p, FUN.argnames, mclapply.optional.args,
       optional.args, subsets, wkdir.out, num.slurm.processes, file = envir.file)

  if (verbose)
    cat("\nR environment for each R instance saved to '", envir.file, "'\n", sep = "")
    
  ################################################################################
  # Write the rscript file that will be called from the MPI code
  ################################################################################
  
  # Gather the names of all packages that will need to be loaded
  # Start with pnlStat
  pkgs <- "pnlStat"

  # Add in parallel if necessary
  if (use.mclapply)
    pkgs <- c(pkgs, "parallel")

  # Add in user requested packages
  if (!is.null(packages))
    pkgs <- c(pkgs, packages)
  
  # Create text for package loading
  pkg.text <- paste("require(", pkgs, ")\n", sep = "")  
    
  # Prepare the header file
  hf.text <- ifelse(is.null(header.file), "# No header file to source\n",
                    paste("source(\"", header.file, "\")\n", sep=""))

  # Rename needed objects if they are present
  if (!is.null(needed.objects))
    mv.text <- c("neededObjects.vec <- ls(pattern = paste(\"neededObjects\", tmpNum, sep=\".\"))\n",
                 "for (v in neededObjects.vec)\n",
                 "  assign(stripExtension(stripExtension(v)), get(v))\n",
                 "rm(list = neededObjects.vec, neededObjects.vec)\n")
  else
    mv.text <- "# No objects passed to the global environment\n"

  # Evaluation of subsets from text...
  get.subset.indexes.text <- "subset.i <- subsets[[process.id + 1]]\n"
  
  if (!is.null(parseJob.args)) {
    if ("text.to.eval" %in% names(parseJob.args)) {
      if (parseJob.args$text.to.eval)
        get.subset.indexes.text <- "subset.i <- eval(parse(text = subsets[[process.id + 1]]))\n"
    }
  }

  # Call to lapply or mclapply
  if (use.mclapply) 
    lapply.text <- paste("X.sub.out <- do.call(mclapply, c(list(X = X.sub, FUN = FUN.p),",
                         "mclapply.optional.args))\n")
  else
    lapply.text <- "X.sub.out <- lapply(X.sub, FUN.p)\n"


  # Write the .R file that will be launched
  cat(# Load packages
      pkg.text,
      
      # Identify the process id passed in from the system call
      "process.id <- as.numeric(commandArgs(TRUE))\n",
      
      # Print the proces id number and the number of processes to the .Rout file
      "print(process.id)\n",
      
      # Source the header file if needed
      hf.text,
      
      # Load the environment file that contains the R objects needed to call lapply or mclapply
      "load(\"", envir.file, "\")\n",
      
      # Rename the 'needed.objects' objects if needed
      mv.text,

      # Evaluate pre-processing expression if it's present
      "if (exists(\"pre.process.expression\"))\n",
      "  eval(pre.process.expression)\n",      
      
      # Identify the subset to be processed by this instance of R
      get.subset.indexes.text,

      # Create the subsetted list and remove the old one (to free up memory in case it's large)
      "X.sub <- X[subset.i]\n",
      "rm(X, subsets, subset.i)\n",

      # Call lapply or mclapply
      lapply.text,

      # Save the results
      "fout <- paste(\"", paste(wkdir.out, "/", jobName, "_completed_\"", sep = ""), 
               ", process.id, \".Rdata\", sep=\"\")\n",      
      "save(X.sub.out, file = fout)\n",

      # Evaluate post-processing expression if it's present
      "if (exists(\"post.process.expression\"))\n",
      "  eval(post.process.expression)\n",
      
      # Print warnings
      "warnings()\n",
      
      sep = "",
      file = rscript.file)

                
  if (verbose)
    cat("\nR script file '", rscript.file, "' written\n", sep = "")

  ################################################################################  
  # Write the launch.c file that will be compiled
  ################################################################################

#  compileMPI(wkdir, rscript.file, mpi.compiler = mpi.compiler, verbose = verbose)
  compileMPI(wkdir, rscript.file, verbose = verbose)

  ################################################################################
  # Write and launch the sbatch script
  ################################################################################

  email.notice <- ifelse(is.null(email.notification), "\n",
                         paste('\n',
                               '#SBATCH --mail-type=END\n',
                               '#SBATCH --mail-user=', email.notification, '\n\n', sep=""))

  # Set the number of processes per node, depending on whether mclapply will be used
  num.processes.text <- ifelse(use.mclapply,
                               "#SBATCH --ntasks-per-node=1\n",
                               paste("#SBATCH -n ", num.slurm.processes, "\n", sep = ""))
  
  # Write the SLURM batch script
  cat('#!/bin/csh\n',
      '\n',
      '#SBATCH -A ', account, '\n',
      '#SBATCH -t ', time.limit.mins, '\n',
      '#SBATCH -N ', numNodes, '\n',
      num.processes.text,
      '#SBATCH -p ', partition, '\n',
      '#SBATCH -o ', sl.out, '\n',
      '#SBATCH -e ', sl.err, '\n',
      email.notice,
      'date "+DateTime %Y-%m-%d %T"\n',
      'source /etc/profile.d/modules.csh\n',
      'module purge\n',
#      'module load pnnl_env\n',
      'module load gcc/4.6.0\n',
      'module load mvapich2/1.7\n',
      'unlimit\n',
      '\n',
      'echo\n',
      'echo "loaded modules"\n',
      'echo\n',
      'module list >& _modules.lis_\n',
      'cat _modules.lis_\n',
      '/bin/rm -f _modules.lis_\n',
      'echo\n',
      'echo "Limits"\n',
      'echo\n',
      'limit\n',
      'echo\n',
      'echo "Environmental Variables"\n',
      'echo\n',
      'printenv\n',
      'echo\n',
      'echo "ldd output"\n',
      'echo\n',
      'ldd ', wkdir, '/launch.o\n',
      '\n',
      'echo "Launch the R jobs"\n',
      'srun --mpi=none ', wkdir, '/launch.o\n',
      'echo "srun is complete"\n',
      '\n',
      'date "+DateTime %Y-%m-%d %T"\n',      
      'echo "Job completion"\n',
      sep = "",
      file = slurm.batch.script)

  # Launch the parallel jobs, redirect stderr to a file
  sb.stdout <- system(paste("sbatch ", slurm.batch.script, " 2>", sb.err, sep = ""), intern = TRUE)

  # Count the number of lines in the stderr of the sbatch command
  n.sb.err <- as.numeric(system(paste("wc -l", sb.err, "| awk '{print $1}'"), intern = TRUE))

  # Check for errors in the sbatch launch
  if (!length(sb.stdout) & n.sb.err) {
    cat("SLURM job failed to launch.  Printing '", sb.err, "':\n", sep = "")
    system(paste("cat", sb.err))
    stop()
  }

  ################################################################################
  # Wait for the sbatch script to complete
  ################################################################################
  
  if (verbose) 
    cat("\n", sb.stdout, ". Launching ", num.slurm.processes,
        ifelse(num.slurm.processes > 1, " instances ", " instance "), "of R.\n",
        "You can check the status of the SLURM job by typing 'squeue -u ", usrName,
        "' at the system prompt.\nWaiting for SLURM job to complete...\n",
        sep = "")

  # Initialize variables for the waiting loop
  continueWaiting <- TRUE
  elapsed.min <- 0
  start.time <- NULL
  timeOut <- FALSE
  notFirst <- FALSE
  wait.counter <- 0
  workReported <- FALSE
  slurm.jobid <- sub("Submitted batch job ", "", sb.stdout)

  # Stop if an error happens, if the job completes successfully, or if the elapsed time expires
  while (continueWaiting & !timeOut) {

    # Wait during each iteration of this loop
    if (notFirst) {
      Sys.sleep(check.interval.sec)
      wait.counter <- wait.counter + 1
    }
    else
      notFirst <- TRUE

    # If any errors occur in the R out files
    if (length(dir(wkdir.logs, pattern = ".Rout"))) {

      # Timing of how long it took for the SLURM job to actually begin
      if (verbose) {
        if (!workReported) {
          if (wait.counter == 0)
            cat("\nWork on SLURM job began immediately after launching\n")
          else {
            lo <- (wait.counter - 1) * check.interval.sec
            hi <- wait.counter * check.interval.sec
            cat("\nWork on SLURM job began between", lo, "and", hi, "seconds after launching\n")
          }
          workReported <- TRUE
        }
      }

      n.Rerr <- as.numeric(system(paste("grep -e 'Error' -e 'Execution halted' ",
                                        wkdir.logs, "/*.Rout | wc -l", sep = ""), intern = TRUE))

      # If there are R errors, cancel the SLURM batch job
      if (n.Rerr) {

        # Kill the SLURM job
        killSlurm(slurm.jobid)

        # Stop the function
        stop("R errors detected in '", wkdir.logs, "/*.Rout'")
        
      }
    }

    # If the slurmLaunch.err file exists, check to see if it has lines.
    if (file.exists(sl.err)) {
      # Get the number of lines in the file
      nlines <- as.numeric(system(paste("wc -l", sl.err, "| awk '{print $1}'"), intern = TRUE))
      if (nlines)
        stop("SLURM job failed, see '", sl.err, "'")
    }

    # Check the SLURM output file, look for the 'Job completion' string
    if (file.exists(sl.out)) {

      # If the last line says 'Job completion', then stop waiting
      last.line <- system(paste("tail -1", sl.out), intern = TRUE)
      if (length(last.line))
        continueWaiting <- !(last.line == "Job completion")

      if (is.null(start.time))
        start.time <- Sys.time()
        
      elapsed.min <- as.numeric(difftime(Sys.time(), start.time, units="m"))

    }

    # Verify the job is still on the queue
    smj <- system(paste("squeue -u", usrName), intern = TRUE)
    smj <- smj[grepl(slurm.jobid, smj)]
    if (length(smj) > 1)
      stop("Unexpected format returned from squeue")

    # Check conditions for which the job should be canceled
    if (length(smj)) {
      
      if (grepl("PartitionTimeLimit", smj)) {
        killSlurm(slurm.jobid)
        stop("'time.limit.min = ", time.limit.mins,
             "' exceeds the acceptable time limit of the ", partition, " partition")
      }
      
      if (grepl("PartitionNodeLimit", smj)) {
        killSlurm(slurm.jobid)
        stop("'numNodes = ", numNodes,
             "' exceeds the acceptable number of nodes for the ", partition, " partition")
      }
    }
    # If the job is no longer in the queue, then wait no more
    else {
      
      if (verbose)
        cat("SLURM job", slurm.jobid, "is no longer in the queue\n")
      
      continueWaiting <- FALSE
    }

    # Verify we haven't exceeded the time limit
    if (elapsed.min > time.limit.mins + 60 * 12) {
      
      timeOut <- TRUE
      killSlurm(slurm.jobid)
      warning("piclapply() timed out after waiting ", round(elapsed.min, 2),
              " minutes for the SLURM job to launch (or complete).\n")
      
    }

  } # while waiting


  ################################################################################
  # Check the R log files for errors or incompletion, get completion times
  ################################################################################

  # Calculate the elapsed time for the SLURM job
  if (verbose) {
    sts <- as.POSIXct(system(paste("grep DateTime", sl.out, "| awk '{print $2 \" \" $3}'"), intern = TRUE))
    slurm.et <- round(as.numeric(difftime(sts[2], sts[1], units = "m")), 2)
    cat("\nSLURM job completed in", slurm.et, "minutes\n\nChecking the log files of the R jobs\n")
  }

  # Look for errors and warnings in the log files
  n.err <- as.numeric(system(paste("grep Error ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                             intern = TRUE))
  n.halt <- as.numeric(system(paste("grep 'Execultion halted' ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                              intern = TRUE))
  n.war <- as.numeric(system(paste("grep Warning ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                             intern = TRUE))
  n.comp <- as.numeric(system(paste("grep 'proc.time()' ", wkdir.logs, "/*.Rout | wc -l", sep = ""),
                              intern = TRUE))

  if (n.err) 
    warning("Errors were identified in ", n.err, " of the R log files in '", wkdir.logs, "'\n")

  if (n.halt)
    warning("Execution was halted for ", n.halt, " of the R log files in '", wkdir.logs, "'\n")

  if (n.war) 
    warning("Warnings were identified in ", n.war, " of the R log files in '", wkdir.logs, "'\n")

  # The number of proc.time()'s in the .Rout files should match the number of processes
  if (n.comp != num.slurm.processes)
    warning("The number of completed .Rout files, ", n.comp,
            " does not match the number of processes, ", num.slurm.processes, ".\n")

  # Get sense of the average completion time in minutes for the individual R jobs
  if (verbose) {

    sys.text <- paste("grep 'proc.time()' -A3 ", wkdir.logs,
                      "/*.Rout | sed -n '3~4p' | awk 'BEGIN {FS = \"[ -]+\"};{print $4}'", sep = "")

    Rout.completion.time <- round(mean(as.numeric(system(sys.text, intern = TRUE)) / 60), 3)
    
    cat("\nThe individual R jobs completed on average in", Rout.completion.time, "minutes\n")

  }

  ################################################################################
  # Concatentate results
  ################################################################################
  
  recombine <- function() {

    # Concatenate the files into a list
    files.to.aggregate <- dir(wkdir.out, full.names = TRUE,
                              pattern = paste(jobName, "_completed_[0-9]*.Rdata", sep = ""))
    
    n.files.to.aggregate <- length(files.to.aggregate)

    # Make sure the files are present
    if (!length(n.files.to.aggregate))
      stop("No completed .Rdata files in '", wkdir.out, "' were available to aggregate")

    # Assign each list file to an object
    for (i in 1:n.files.to.aggregate) 
      assign(paste("f", i, sep = ""), load.object(files.to.aggregate[i]))
  
    # Combine the all using a single call to c()
    combine.text <- paste("c(", paste(paste("f", 1:n.files.to.aggregate, sep = ""),
                                      collapse = ", "), ")",
                          sep = "")
      
    out <- eval(parse(text = combine.text))

    # Remove individual files
    rm(list = paste("f", i, sep = ""))

    # Restore the original ordering and names of the list
    out <- out[as.character(1:lenX)]
    names(out) <- orig.X.names

    return(out)
                 
  } # recombine

  if (verbose) 
    cat("\nRecombining completed R jobs (and reordering to match original list order).\n",
        "Completed in ")

  Xout <- timeIt(recombine(), verbose = verbose)
  
  
  ################################################################################
  # Create tarball of the SLURM job files and remove the directory
  ################################################################################

  if (tarball.file != "none") {
    system(paste("cd ", userScratch, "; tar -Ppczf ",
                 tarball.file, " ", stripPath(wkdir), sep = ""))
    if (verbose)
      cat("\n'", tarball.file, "' created\n", sep = "")
  }
  
  if (remove.working.dir) {
    system(paste("rm -r", wkdir))
    if (verbose)
      cat("\n'", wkdir, "' removed\n", sep = "")
  }


  ################################################################################
  # Return results
  ################################################################################  

  # Restore default options
  options(op)

  if (verbose) {
    cat("\nTotal elapsed time for piclapply():",
        round(as.numeric(difftime(Sys.time(), s.time, unit = "m")), 2),
        "minutes\n")
  }
  
  # Return the results from piclapply
  return(Xout)

} # piclapply

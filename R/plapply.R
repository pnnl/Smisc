##' Simple parallelization of lapply
##'
##' Parses a large list into subsets and submits a separate batch R job that calls \code{\link{lapply}}
##' on the  subset.  \code{plapply} has some features that may not be readily available in
##' other parallelization functions like \code{\link{mclapply}} and \code{\link{parLapply}}:
##' \itemize{
##' \item The \code{.Rout} files produced by each R instance are easily accessible
##' for convenient debugging of errors or warnings.  The \code{.Rout} files
##' can also serve as an explicit record of the work that
##' was performed by the workers
##' \item Three options are available for the ordering of the processing of the list elements:
##' the original list order, randomized, or collated (first-in-first-out).
##' \item In each R instance, pre-processing or post-processing steps can be performed
##' before and after the call to \code{\link{lapply}}}
##' These pre-processing and post-processing steps can depend
##' on the instance of R, such that each instance can be treated differently, if desired.
##' These features give greater control over the computing process, which can be especially useful for large jobs.
##'
##' @details  \code{plapply} applies \code{FUN} to each element of the list \code{X} by
##' parsing the list into \code{njobs} lists of equal (or almost equal) size
##' and then applies \code{FUN} to each sublist using \code{\link{lapply}}.
##' 
##' A separate batch instance of R is launched for each sublist, thus utilizing
##' another core of the machine. After the jobs complete, the \code{njobs}
##' output lists are reassembled. The global environments for each batch instance
##' of R are created by writing/reading data to/from disc.
##'
##' If \code{collate = TRUE} or \code{random.seed = Integer value}, the output
##' list returned by \code{plapply} is reordered to reflect the original
##' ordering of the input list, \code{X}.
##'
##' An object called \code{process.id} (consisting of an integer indicating the
##' process number) is available in the global environment of each instance of
##' R.
##'
##' Each instance of R runs a script that performs the following steps:
##'
##' \enumerate{
##'
##' \item Any other packages indicated in the \code{packages} argument are
##' loaded via calls to \code{library()}
##'
##' \item The \code{process.id} global variable is assigned to the global
##' environment of the R instance (having been passed
##' in via a command line argument)
##'
##' \item The header file (if there is one) is sourced
##'
##' \item The expression \code{pre.process.expression} is evaluated if an
##' object of that name is present in the global environment. The object
##' \code{pre.process.expression} may be passed in via the header file or via
##' \code{needed.objects}
##'
##' \item \code{\link{lapply}} is called on the sublist, the sublist is called
##' \code{X.i}
##'
##'\item The expression \code{post.process.expression} is evaluated if an
##' object of that name is present in the global environment.  The object
##' \code{post.process.expression} may be passed in via the header file or via
##' \code{needed.objects}
##'
##' \item The output returned by \code{lapply} is assigned to the object
##' \code{X.i.out}, and is saved to a temporary file
##' where it will be collected after all jobs have completed
##'
##' \item Warnings are printed
##' }
##'
##' If \code{njobs = 1}, none of the previous steps are executed, only this
##' call is made:  \code{lapply(X, FUN, ...)}
##'
##' @export
##' @param X A list or vector, each element of which will be the input to \code{FUN}
##'
##' @param FUN A function whose first argument is an element of \code{X}
##'
##' @param \dots Additional named arguments to \code{FUN}
##' 
##' @param njobs The number of jobs (subsets).  Defaults to one less than the
##' number of cores on the machine.
##'
##' @param packages Character vector giving the names of packages that will be
##' loaded in each new instance of R, using \code{\link{library}}.
##'
##' @param header.file Text string indicating a file that will be initially
##' sourced prior calling \code{\link{lapply}} in order to create an
##' 'environment' that will satisfy all potential dependencies for \code{FUN}.
##' If \code{NULL}, no file is sourced.
##'
##' @param needed.objects Character vector giving the names of objects which
##' reside in the evironment specified by \code{needed.objects.env} that may be
##' needed by \code{FUN} which are loaded into the global environment of each
##' new instance of R that is launched.  If \code{NULL}, no additional objects
##' are passed.
##'
##' @param needed.objects.env Environment where \code{needed.objects} reside.
##' This defaults to the environment in which \code{plapply} is called.
##'
##' @param workDir Character string giving the name of the working directory that
##' will be used for for the files needed to launch the separate instances of R.
##'
##' @param clobber Logical indicating whether the directory designated by \code{workDir}
##' will be overwritten if it exists and contains files.  If \code{clobber = FALSE},
##' and \code{workDir} contains files, \code{plapply} throws an error.
##'
##' @param max.hours The maximum number of hours to wait for the \code{njobs}
##' to complete.
##'
##' @param check.interval.sec The number of seconds to wait between checking to
##' see whether all \code{njobs} have completed.
##'
##' @param collate \code{= TRUE} creates a 'first-in-first-out' processing order of
##' the elements of the input list \code{X}.  This logical is passed to the
##' \code{collate} argument of \code{\link{parseJob}}.
##'
##' @param random.seed An integer setting the random seed, which will result in
##' randomizing the elements of the list assigned to each job. This is useful
##' when the computing time for each element varies significantly because it
##' helps to even out the run times of the parallel jobs. If \code{random.seed
##' = NULL}, no randomization is performed and the elements of the input list
##' are subdivided sequentially among the jobs.  This variable is passed to the
##' \code{random.seed} argument of \code{\link{parseJob}}. If \code{collate = TRUE},
##' no randomization is performed and \code{random.seed} is ignored.
##'
##' @param rout A character string giving the name of the file to where all of the \code{.Rout} files
##' will be gathered.  If \code{rout = NULL}, the \code{.Rout} files are not gathered, but left
##' alone in \code{workDir}.
##'
##' @param clean.up \code{= TRUE} will delete the working directory.
##'
##' @param verbose \code{= TRUE} prints messages which show the progress of the
##' jobs.
##'
##' @return A list equivalent to that returned by \code{lapply(X, FUN, ...)}.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{parLapplyW}}, \code{\link{dfplapply}}, \code{\link{parLapply}}, \code{\link{mclapply}}
##'
##' @keywords misc
##'
##' @examples
##' # Create a simple list
##' a <- list(a = rnorm(10), b = rnorm(20), c = rnorm(15), d = rnorm(13),
##'           e = rnorm(15), f = rnorm(22))
##'
##' # Some objects that will be needed by f1:
##' b1 <- rexp(20)
##' b2 <- rpois(10, 20)
##'
##' # The function
##' f1 <- function(x) mean(x) + max(b1) - min(b2)
##'
##' # Call plapply
##' res1 <- plapply(a, f1, njobs = 2, needed.objects = c("b1", "b2"),
##'                 check.interval.sec = 0.5, max.hours = 1/120,
##'                 workDir = "example1", rout = "example1.Rout",
##'                 clean.up = FALSE)
##'
##' print(res1)
##'
##' # Look at the collated 'Rout' file
##' more("example1.Rout")
##'
##' # Look at the contents of the working directory
##' dir("example1")
##'
##' # Remove working directory and Rout file
##' unlink("example1", recursive = TRUE, force = TRUE)
##' unlink("example1.Rout")
##'  
##' # Verify the result with lapply
##' res2 <- lapply(a, f1)
##'
##' # Compare results
##' identical(res1, res2)

plapply <- function(X, FUN, ...,
                    njobs = parallel::detectCores() - 1,
                    packages = NULL,
                    header.file = NULL,
                    needed.objects = NULL,
                    needed.objects.env = parent.frame(),
                    workDir = "plapply",
                    clobber = TRUE,
                    max.hours = 24,
                    check.interval.sec = 1,
                    collate = FALSE,
                    random.seed = NULL,
                    rout = NULL,
                    clean.up = TRUE,
                    verbose = FALSE) {

  # Argument checks
  stopifnotMsg(# X
               is.vector(X),
               "'X' must be list or a vector",
               
               # FUN
               is.function(FUN),
               "'FUN' must be a function",
               
               # njobs
               if (is.numeric(njobs)) (njobs > 0) & (njobs %% 1 == 0) else FALSE,
               "'njobs' must be a positive, whole number",
               
               # packages
               if (!is.null(packages)) is.character(packages) else TRUE,
               "'packages' must be a character vector or NULL",
               
               # header.file
               if (!is.null(header.file)) {
                  if (is.character(header.file) & length(header.file) == 1) {
                    file.exists(header.file)
                  } else FALSE
               } else TRUE,
               "'header.file' must be a character string of an existing file or NULL",
               
               # needed.objects
               if (!is.null(needed.objects)) is.character(needed.objects) else TRUE,
               "'needed.objects' must be a character vector or NULL",
               
               # needed.objects.env
               is.environment(needed.objects.env),
               "'needed.objects.env' must be an environment",
               
               # workDir
               is.character(workDir) & (length(workDir) == 1),
               "'workDir' must be a character string",
               
               # clobber
               is.logical(clobber) & (length(clobber) == 1),
               "'clobber' must be TRUE or FALSE",
               
               # max.hours
               if (is.numeric(max.hours)) max.hours > 0 else FALSE,
               "'max.hours' must be numeric and positive",
               
               # check.interval.sec
               if (is.numeric(check.interval.sec)) check.interval.sec > 0 else FALSE,
               "'check.interval.sec' must be numeric and positive",
               
               # collate
               is.logical(collate) & (length(collate) == 1),
               "'collate' must be TRUE or FALSE",
               
               # random.seed
               if (!is.null(random.seed)) is.numeric(random.seed) & (length(random.seed) == 1) else TRUE,
               "'random.seed' must be numeric or NULL",
               
               # rout
               if (!is.null(rout)) is.character(rout) & (length(rout) == 1) else TRUE,
               "'rout' must be a character string or NULL",
               
               # clean.up
               is.logical(clean.up) & (length(clean.up) == 1),
               "'clean.up' must be TRUE or FALSE",
               
               # verbose
               is.logical(verbose) & (length(verbose) == 1),
               "'verbose' must be TRUE or FALSE")

  # Set njobs to an integer value, >= 1
  njobs <- as.integer(max(1, njobs))

  ################################################################################
  # Run regular lapply if only one job requested
  ################################################################################
  if (njobs == 1) {

    return(lapply(X, FUN, ...))

  } # njobs == 1

  # Check the platform
  os <- .Platform$OS.type
  
  if (!(os %in% c("unix", "windows"))) {
    stop("plapply() for njobs > 1 is currently only supported on the Unix and Windows platforms\n")
  }
  
  os.win <- os == "windows"

  # Check the needed.objects for reserved names
  if (!is.null(needed.objects)) {

    # Make sure 'needed.objects' does not include the following
    reserved.global.objects <- c("X.i", "FUN.p", "FUN.argnames", "optional.args", "process.id")

    conflict.objects <- reserved.global.objects[reserved.global.objects %in% needed.objects]

    if (length(conflict.objects)) {
      stop("The following are reserved objects for 'plapply', and should not be included ",
           "in the 'needed.objects' argument:\n '", paste(conflict.objects, collapse = "', '"), "'\n")
    }
  }

  # Calculate the list length
  lenX <- length(X)

  if (lenX < njobs) {
    warning("Number of requested job exceeds the number of elements in the list.\n",
            "'njobs' will be reduced to", lenX)
    njobs <- lenX
  }

  # Match function
  FUN <- match.fun(FUN)

  # Get the optional args to FUN into a list
  optional.args <- list(...)

  # Now add needed.objects into this environment so they can be saved from here, along with other objects
  # in this environment.  Add suffix to their names so they will be unique from any other objects
  # in the environment of the plapply() function
  if (!is.null(needed.objects)) {

    # Add suffix to the names
    for (v in needed.objects) {

      # Verify the needed objects exist
      if (!exists(v, envir = needed.objects.env)) {
        stop("The object '", v, "' does not exist in the environment specified by 'needed.objects.env'")
      }
      
      # Add a suffix to the needed object name
      assign(paste(v, "neededObjects", sep = "."), get(v, pos = needed.objects.env))
    }

    needed.objects <- paste(needed.objects, "neededObjects", sep = ".")

  }

  # Match the arguments that actually exist in FUN
  FUN.argnames <- names(formals(FUN))

  if (length(optional.args)) {

    # If any optional arguments are provided that are not in FUN
    if (!all(names(optional.args) %in% FUN.argnames[-1])) {
      stop("Optional arguments do not match the optional arguments of FUN")
    }

    FUN.p <- function(x) {
      xList <- eval(parse(text = paste("list(", FUN.argnames[1], "= x)", sep = "")))
      do.call("FUN", c(xList, optional.args))
    }

  }
  else {
    FUN.p <- FUN
  }

  # Identify how to parse the job
  subsets <- parseJob(lenX, njobs, collate = collate, random.seed = random.seed)

  ################################################################################
  # Create a temporary directory with a unique name where the jobs will run
  ################################################################################

  # If the workDir directory exists, check to see whether it has files:
  if (file.exists(workDir)) {

    # Check whether there are files in 'workDir'
    if (length(dir(workDir, all.files = TRUE))) {
      
      if (clobber) {
        
        if (verbose) {
          cat("Removing existing files from working directory '", workDir, "'\n", sep = "")
        }
          
        unlink(workDir, recursive = TRUE, force = TRUE)
        dir.create(workDir)
        
      }

      # Throw the error if there is content in 'workDir' and clobber = FALSE
      else {
        stop("There are files in '", workDir, "'.  To remove them automatically, set 'clobber = TRUE'")
      }
      
    } # If there are files in 'workDir'
    
  } # If 'workDir' exists
  
  # 'workDir' does not exist and needs to be created
  else {
      
    if (verbose) {
      cat("Creating working directory '", workDir, "'\n", sep = "")
    }
   
    dir.create(workDir)
  }
  
  # Base name for files
  rsf <- paste(workDir, "/", "r", sep = "")

  ################################################################################
  # Prepare text strings that will be written to the .R files that are separately launched
  ################################################################################
  
  # Load packages as requested
  if (is.null(packages)) {
    pk.text <- "# No packages requested\n"
  }
  else {
    pk.text <- paste("library(", packages, ")\n", sep = "")
  }
  
  # Prepare the header file
  if (!is.null(header.file)) {
    hf.text <- paste("source(\"", header.file, "\")\n", sep = "")
  }
  else {
    hf.text <- "# No header file included\n"
  }

  # Rename needed objects if they are present (strip the suffix)
  if (!is.null(needed.objects)) {
    mv.text <- c("neededObjects.vec <- ls(pattern = \"neededObjects\")\n",
                 "for (v in neededObjects.vec) {\n",
                 "  assign(Smisc::stripExtension(v), get(v))\n",
                 "}\n",
                 "rm(list = neededObjects.vec, neededObjects.vec)\n")
  }
  else {
    mv.text <- "# No needed.objects present\n"
  }

  ################################################################################
  # Write the .R files and launch the separate instances
  ################################################################################
  
  # Initialize output files
  list.outputs <- NULL

  # Gather all the .R filenames
  fnames <- NULL
  
  for (i in 1:njobs) {

    # File name for the R code that will be called by launching a separate instance of R
    fname <- paste(rsf, "_", padZero(i, nchar(njobs)), ".R", sep = "")
    fnames <- c(fnames, fname)

    # The subset of the list that will be operated on
    subset <- subsets[[i]]

    # The filename for all the input to the lapply
    list.input.i <- paste(stripExtension(fname), "_inputData.Rdata", sep = "")

    # The filename for the output from the lapply
    list.output.i <- paste(stripExtension(fname), "_outputData.Rdata", sep = "")

    # Concatentate output filenames for use in collecting the results later
    list.outputs <- c(list.outputs, list.output.i)

    # Save the environment that will be needed.  This approach writes more data--but it ensures
    # processes won't be competing trying to read the same file
    X.i <- X[subset]
    save(list = needed.objects, X.i, FUN.p, FUN.argnames, optional.args, file = list.input.i)

    # Write the .R file that will be launched
    cat(# Load packages
        "## Load requested packages\n",
        pk.text,

        # Source the header file (if there is one)
        "\n## Source the header file if there is one\n",
        hf.text,

        # Set the process id variable
        "\n## Set the process id variable\n",
        "process.id <- ", i, "\n",

        # Load the file that will place all the needed objects in the Global R environment
        "\n## Load the file with all the necessary objects\n",
        "load(\"", list.input.i, "\")\n",

        # Rename 'needed.objects' (if they're present)
        "\n## Rename 'needed.objects' if they are present\n",
        mv.text,

        # Evaluate pre-processing expression if it's present
        "\n## Evaluate pre-processing expression if it's present\n",
        "if (exists(\"pre.process.expression\")) {\n",
        "  eval(pre.process.expression)\n",
        "}\n",

        # Process this subset of the list
        "\n## Process the subset of the list using lapply()\n",
        "X.i.out <- lapply(X.i, FUN.p)\n",

        # Evaluate post-processing expression if it's present
        "\n## Evaluate pre-processing expression if it's present\n",        
        "if (exists(\"post.process.expression\")) {\n",
        "  eval(post.process.expression)\n",
        "}\n",

        # Save this output
        "\n## Save the results\n",
        "save(X.i.out, file = \"", list.output.i, "\")\n",

        # Print warnings
        "\n## Print warnings if any were incurred\n",
        "warnings()\n",

        sep = "",
        file = fname)

    # Now launch the i_th job, the annoying 'nohup.out' is routed to /dev/null
    if (os.win) {
          
      Rcmd <- paste(file.path(R.home("bin"), "Rcmd.exe"), "BATCH --no-restore --no-save", fname, paste(fname, "out", sep = ""))
      shell(Rcmd, mustWork = TRUE, wait = FALSE, translate = TRUE)
          
    } 
    else {
        
      Rcmd <- paste("nohup", file.path(R.home("bin"), "R"), "CMD BATCH --no-restore --no-save",
                    fname, paste(fname, "out", sep = ""), "> /dev/null 2>&1 &")
      system(Rcmd)
             
    }

    if (verbose) {
      cat("Launching job", i, "\n")
    }

  } # for

  ################################################################################
  # Function for gathering the .Rout files into a single file
  ################################################################################
  
  gatherRout <- function() {
  
    # gather all .Rout files into a single .Rout file
    if (!is.null(rout)) {

      routFnames <- paste(stripExtension(fnames), "Rout", sep = ".")

      # Create streams for each file
      for (i in 1:njobs) {

         # Create the file contents with a header
         fileContents <- c("################################################################################",
                           stripPath(routFnames[i]),
                           "################################################################################",
                           readLines(routFnames[i]),
                           "")

         # Assign it to a unique object name
         assign(paste("routFile", i, sep = ""), fileContents)

      }

      # Now bind them together and write the output
      writeLines(qbind(paste("routFile", 1:njobs, sep = ""), type = "c"), con = rout)

      if (verbose) {
        cat("The ", length(routFnames), " '.Rout' files were gathered and written to '", rout, "'\n", sep = "")
      }
        
    } # If we are gathering Rout files

  } # gatherRout()

  ################################################################################  
  # Wait for the jobs to finish
  ################################################################################  
  
  njobs.finished <- 0
  elapsed.hours <- 0
  start.time <- Sys.time()

  if (verbose) {
    cat("Waiting for jobs to complete...\n")
  }

  while ((njobs.finished < njobs) & (elapsed.hours < max.hours)) {

    Sys.sleep(check.interval.sec)

    if (os.win) {

      orig.opt <- options(warn = -1)

      njf.1 <- length(shell(paste('findstr /M "proc.time()" ', workDir, '\\*.Rout', sep = ""), intern = TRUE))
      njf.2 <- length(shell(paste('findstr /M "Execution halted" ', workDir, '\\*.Rout', sep = ""), intern = TRUE))

      options(orig.opt)

    }
    else {

      njf.1 <- as.numeric(system(paste("grep -l 'proc.time()' ", workDir, "/*.Rout | wc -l",
                                       sep = ""), intern = TRUE))

      njf.2 <- as.numeric(system(paste("grep -l 'Execution halted' ", workDir, "/*.Rout | wc -l",
                                       sep = ""), intern = TRUE))

    }

    njobs.finished <- njf.1 + njf.2
    elapsed.hours <- as.numeric(difftime(Sys.time(), start.time, units = "h"))

  }

  # If one of the jobs failed
  if (njf.2) {

    # Gather the Rout files
    gatherRout()
    
    stop("It appears that ", njf.2, " of the parallelized jobs failed.\n",
         "Look at ", if (!is.null(rout)) paste("'", rout, "' or ", sep = ""),
         "the '*.Rout' files of the separate jobs in '", workDir, "'.\n")
  }

  ################################################################################
  # If they all finished and we didn't time out
  ################################################################################
  if (njobs.finished == njobs) {

    # Load the outputs to their own objets, one at a time
    for (f in list.outputs) {
        
      assign(paste(stripExtension(stripPath(f)), "loaded", sep = "_"), loadObject(f))
      
      if (verbose) {
        cat("Adding data from '", f, "'\n", sep = "")
      }
      
    }

    # Concatenate the objects into a single list
    Xout <- qbind(paste(stripPath(stripExtension(list.outputs)), "loaded", sep = "_"), type = "c")
    
    # If collated or random, need to restore list to original order
    if (collate | !is.null(random.seed)) {
      Xout <- Xout[order(unlist(subsets))]
    }

    # Some checks
    if (!all(names(Xout) == names(X))) {
      warning("The names of the output list do not match those of the input list, 'X'\n")
    }

    # Gather the Rout files
    gatherRout()

    # Delete the working directory
    if (clean.up) {

      if (verbose) {
        cat("Removing the working directory '", workDir, "'\n", sep = "")
      }

      unlink(workDir, recursive = TRUE, force = TRUE)

    }


  } # if (njobs.finished == njobs) {

  else {
    stop("Jobs did not complete in the maximum waiting time of ",
         round(max.hours, 5), " hours.",
         "\nOutput files were not concatenated and '", workDir, "' was not deleted.\n")
  }
  
  return(Xout)

} # plapply

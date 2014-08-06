# A parallelized (more flexible) version of dapply that uses plapply
# FUN must opererate on a single row of X

dfplapply <- function(X, FUN, ...,
                      output.df = FALSE,
                      packages = NULL,
                      header.file = NULL,
                      needed.objects = NULL,
                      needed.objects.env = parent.frame(),
                      jobName = "dfplapply",
                      njobs = 7,
                      max.hours = 24,
                      check.interval.sec = 30,
                      collate = FALSE,
                      random.seed = NULL,
                      clean.up = TRUE,
                      rout = !clean.up,
                      verbose = FALSE) {


  if (!is.data.frame(X))
    stop("'", deparse(substitute(X)), "' is not a data frame\n")    

  # Process it with plapply
  X.out <- plapply(df2list(X, out.type = "data.frame"), FUN, ...,
                   packages = packages,
                   header.file = header.file,
                   needed.objects = needed.objects,
                   needed.objects.env = needed.objects.env,
                   jobName = jobName,
                   njobs = njobs,
                   max.hours = max.hours,
                   check.interval.sec = check.interval.sec,
                   collate = collate,
                   random.seed = random.seed,
                   clean.up = clean.up,
                   rout = rout,
                   verbose = verbose)

  # Collapse results to a data frame if requested
  if (output.df) 
    X.out <- list2df(X.out)

  return(X.out)
  
} # dfplapply



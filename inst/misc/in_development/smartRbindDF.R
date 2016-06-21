################################################################################
#
# Filename:   smartRbindMat.R
# Creator:    Landon Sego
# Date Created:  June 2007
# Description/Purpose:  Row bind data frames whose column names may not be the
#                       same.  Produces a data frame with a union of the
#                       column names.  Row names are reproduced if they exist,
#                       otherwise, unique rownames are created.
#
# I don't think I finished this function.  (LHS, 2011-11-03)
#
# Modified by:
# Date modified:
# Brief summary of changes:
#
# Modified by:
# Date modified:
# Brief summary of changes:
# 
# Modified by:
# Date modified:
# Brief summary of changes:
#
#
################################################################################

smartRbindDF <- function(..., distinguish=FALSE) {

  # Preliminary handling of the objects
  dfs <- list(...)
  obj.names <- as.character(substitute(list(...)))[-1]
  names(dfs) <- obj.names

  # If any of the objects are NULL, remove them from the list

  # Verify they are all dataframes and obtain the union of the
  # column names.  If rownames are missing, they are assigned a number
  # with the object name as a prefix, separated by a ":"
  union.colnames <- NULL
  cnt <- 0

  check.objects <- function(x) {

    # Increment the counter (and record the value outside the function)
    assign("cnt", cnt + 1, inherits=TRUE)

    # This allows the possibility that one of objects was NULL
    if (!is.null(x)) {
      
      # Verify we're working with a data frame
      if (!is.data.frame(x))
          stop("'", obj.names[cnt], "' is not a data frame\n")

      # Make sure the names are not missing
      if (is.null(colnames(x)))
        stop("'", obj.names[cnt], "' does not have column names\n")
      
      # Create unique identifiers of the variables which include the class
      x.classes <- unlist(lapply(x, class))
      cnames <- paste(names(x.classes), x.classes, sep=".")
    
      # Column names must be unique
      if (length(unique(cnames)) < length(cnames))
        stop("'", obj.names[cnt], "' does not have unique column names\n")
  
      # Find the union of all the column names, record the value outside the function
      assign("union.colnames", union(union.colnames, cnames), inherits=TRUE)
  
      # Add in rownames if they're not present
      if (distinguish) {
        if (is.null(rownames(x)))
          rownames(x) <- paste(obj.names[cnt], 1:NROW(x), sep=":")
      }

    } # if (!is.null(x))

    x
        
  } # check.objects()

  # Makes it easier to trace an error
  dfs <- try(lapply(dfs, check.objects), silent=TRUE)
  if (class(dfs) == "try-error")
    stop(dfs)

  # Add in NA columns where necessary and reorder to give the same ordering
  addinNA <- function(x) {

    # Identify the differences
    missing.cols <- setdiff(union.colnames, colnames(x))

    # Add in missing columns
    if (length(missing.cols)) {
      for (mc in missing.cols) {
        x <- cbind(x, rep(NA,NROW(x)))
        colnames(x)[NCOL(x)] <- mc
      }
    }

    # Prevent matrices from 1 row from being collapsed into a named vector
    if (NROW(x)==1)
      matrix(x[,union.colnames], nrow=1,
             dimnames=list(rownames(x),union.colnames))
    else
      x[,union.colnames]

  } # addinNA

  # Makes it easier to trace an error
  dfs <- try(lapply(dfs, addinNA), silent=TRUE)
  if (class(dfs) == "try-error")
    stop(dfs)

  # Text that, when executed, will bind all the matrices together with
  # a single call to rbind()
  rbind.text <- paste("rbind(",
                      paste(paste("dfs[[", names(dfs), "]]", sep='"'),
                            collapse=","),
                      ")", sep="")

  # return the result
  eval(parse(text=rbind.text))
  
} # smartRbindDF()


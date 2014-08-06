# Flexible data aquisition
dataIn <- function(data) {

  # data can be: a .csv or .Rdata file, a data frame, or a textstring indicating a data object in a particular package, using the
  # nomenclature "packageName::dataSetName".

  fail <- FALSE
  
  # If 'data' is a character string, then read or load the file
  if (is.character(data)) {

    if (length(data) == 1) {

      # If it has a ::, load the data from the package
      if (grepl("::", data)) {

        splt <- strsplit(data, "::")
        pkg <- splt[[1]][1]
        dat <- splt[[1]][2]
        
        e1 <- new.env()
        val <- data(list=dat, package=pkg, envir=e1)
        d <- get(val, envir=e1)

      }

      # otherwise, it's a file
      else {
        
        ext <- tolower(getExtension(data))

        if (ext == "csv")
          d <- factor2character(read.csv(data))
        
        else if (ext == "rdata") 
          d <- load.object(data)

        else
          fail <- TRUE
      }
        
    } # if (length(data) == 1)
    
    else
      fail <- TRUE
      
  }

  # Otherwise, verify it's a data frame
  else if (is.data.frame(data)) 
    d <- data

  # Otherwise, it should be a list of data frames
  else if (is.list(data)) {
    if (all(unlist(lapply(data, is.data.frame))))
      d <- data
    else
      fail <- TRUE
  }

  else
    fail <- TRUE

  if (fail)
    stop("'data' must be a data frame, a list of data frames, or a single text string specifying\n",
         "either a valid .Rdata or .csv filename or a data set in a package using the syntax\n",
         "'packageName::dataSetName'.\n")

  # return the data frame
  return(d)
  
} # dataIn

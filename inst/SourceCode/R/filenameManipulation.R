# strip off the filename extension at the end
# It assumes that the extension is the string that
# follows the last period in a filename
stripExtension <- function(vec) {

  # Verify that 'vec' is a character vector
  if (!is.character(vec))
    stop("'", deparse(substitute(vec)), "' must be a character vector.\n")

  # If the last character is a period . then flag it so that nothing will be returned
  check.for.ending.period <- function(x) {

     if (!is.na(x)) {
       n <- nchar(x)
       if (substr(x, n, n) == ".")
         x <- paste(x, "BAD-EXTENSION-NO-ONE-WOULD-NAME-THEIR-EXTENSION-THIS", sep="")
     }

     return(x)
     
  } # check.for.ending.period

  names.vec <- names(vec)
  vec <- tapply(vec, 1:length(vec), check.for.ending.period)

  # Function to rejoin any filenames that may have more than one
  # period
  rejoin <- function(v1) {

    if ((len <- length(v1)) > 1)
      return(paste(v1[1:(len-1)], collapse="."))

    else
      return(v1)
    
  } # end rejoin

  # If paths exist in the filename, we don't want to cut at "." that are in directories...
  # Strip the extension in the last element of the sub-list when we split by "/"
  sE <- function(v2) {
    
    if (length(v2)==1) {
      if (is.na(v2))
        return(NA)
    }
    
    v2[length(v2)] <- unlist(lapply(strsplit(v2[length(v2)], "\\."), rejoin))
    paste(v2, collapse="/")
  }

  result <- unlist(lapply(strsplit(vec, "/"), sE))
  names(result) <- names.vec

  return(result)
  
} # end stripExtension()


# Grabs the last string of text that follows 'split.char'
grabLast <- function(vec, split.char) {

  # Check the split.char
  if (!is.character(split.char))
    stop("'split.char' argument must be an single character.\n")
  if (length(split.char) != 1)
    stop("'split.char' argument must be an single character.\n")
  
  # Verify that 'vec' is a character vector
  if (!is.character(vec))
    stop("'", deparse(substitute(vec)), "' must be a character vector.\n")

  # Prerve the names for later
  names.vec <- names(vec)
  
  # If the last character is the split.char then flag it so that nothing will be returned
  check.for.ending.char <- function(x) {

     if (!is.na(x)) {
       n <- nchar(x)
       if (substr(x, n, n) == split.char)
         x <- paste(x, "BAD-ENDING-CHAR-NO-ONE-WOULD-DO-THIS", sep="")
     }

     return(x)
     
  } # check.for.ending.char

  vec <- tapply(vec, 1:length(vec), check.for.ending.char)
  
  getLast <- function(v1) {

    # For NA's, nothing after the split.char, and ""
     if ((lv <- length(v1)) <= 1) {

       # For ""
       if (!length(v1))
         return("")
       # For NA
       if (is.na(v1))
         return(NA)
       # For nothing after the split.char
       else 
         return("")
    }
    # If string doesn't end with the split.char
    else if (v1[lv] != "BAD-ENDING-CHAR-NO-ONE-WOULD-DO-THIS")
      return(v1[lv])
    # If it does end with the split.char
    else
      return("")

  } # getLast

  result <- unlist(lapply(strsplit(vec, split.char, fixed=TRUE), getLast))
  names(result) <- names.vec

  return(result)

} # grabLast

    
getExtension <- function(vec) {

  grabLast(vec, ".")
  
} # getExtension

stripPath <- function(vec) {

  # If only a filename with no path comes in, we should still return the filename, so prepend
  # the 'vec' with '/'
  vec1 <- paste("/", vec, sep="")
  vec1[is.na(vec)] <- NA
  names(vec1) <- names(vec)

  # Grab the text string that follows the last '/'
  grabLast(vec1, "/")
  
} # stripPath

    
# Extracts the path only from a filename
getPath <- function(vec) {

  # Verify that 'vec' is a character vector
  if (!is.character(vec))
    stop("'", deparse(substitute(vec)), "' must be a character vector.\n")

  names.vec <- names(vec)

  # If no slashes, it should return "."
  # If ends with a slash, it should return the whole string minus the slash at the end
  preprocess <- function(x) {

    if (is.na(x))
      return(NA)
    else if (x == "")
      return("/")
    else if (!length(grep("/", x)))
      return(paste("./", x, sep=""))
    else if (substr(x, nchar(x), nchar(x)) == "/")
      return(paste(x, "tmpJunk", sep=""))
    else
      return(x)

  } # preprocess

  vec <- tapply(vec, 1:length(vec), preprocess)

  # Function to rejoin any filenames that may have more than one
  # period
  rejoin <- function(v1) {

    if (length(v1) == 1) {
      if (is.na(v1))
        return(NA)
    }
    
    if ((len <- length(v1)) > 1)
      return(paste(v1[1:(len-1)], collapse="/"))

    else
      return(v1)
    
  } # end rejoin

  # Return the results
  out <- unlist(lapply(strsplit(vec, "/"), rejoin))
  names(out) <- names.vec

  out
  
} # getPath


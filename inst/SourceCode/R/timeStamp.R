"timeStamp" <-
function(description,extension) {

  # Verify that inputs are character
  if (!(is.character(description) & is.character(extension)))
    stop("Arguments to timeStamp() must be character.\n")
  
  # Returns a text string of the form:
  # description_YYYY-MM-DD_HHMMSS.extension
  
  dtime <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  
  return(paste(description, "_", dtime, ".", extension,sep=""))

}  

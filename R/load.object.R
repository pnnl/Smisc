##' Loads and returns the first object in an ".Rdata" file
##' 
##' Loads and returns the first object in the ".Rdata" file.  This is useful
##' for naming the object in the ".Rdata" something other than the name it was
##' saved as.
##' 
##' If the '.Rdata' file contains more than one object, only the first object
##' is returned and a warning is produced.
##' 
##' @usage load.object(Rdata.file)
##' @param Rdata.file A character string containing the '.Rdata' filename
##' @return The first object in the '.Rdata' file.
##' @author Landon Sego
##' @keywords misc
##' @examples
##' 
##' # Make an example object
##' x <- data.frame(a=1:10, b=rnorm(10))
##' 
##' # Create a unique filename with a time stamp
##' # imbedded in the filename
##' file.name <- timeStamp("demo_load_object", "Rdata")
##' 
##' # Save and reload the file
##' save(x, file=file.name)
##' y <- load.object(file.name)
##' print(y)
##' 
##' # Delete the saved object
##' unlink(file.name)
##' 
##' 
load.object <- function(Rdata.file) {

  obj.in <- load(Rdata.file)

  if (length(obj.in) > 1)
    warning(deparse(substitute(Rdata.file)), " contains more than one object. ",
            "Only object '", obj.in[1], "' will be returned.\n", sep="")

  return(get(obj.in))
  
} # end load.object

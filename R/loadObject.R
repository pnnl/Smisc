##' Loads and returns the object(s) in an ".Rdata" file
##'
##' Loads and returns the object(s) in the ".Rdata" file.  This is useful
##' for naming the object(s) in the ".Rdata" something other than the name it was
##' saved as.
##'
##' @export
##'
##' @param RdataFile A character string containing the '.Rdata' filename
##' @return If \code{RdataFile} contains only one object, then that object is returned.
##' However, if \code{RdataFile} contains multiple objects, a list of those objects is
##' returned. The names of the list correspond to the names of the saved objects.
##' @author Landon Sego
##' @keywords misc
##' @examples
##'
##' # Make an example object
##' x <- data.frame(a = 1:10, b = rnorm(10))
##'
##' # Create a unique filename with a time stamp
##' # imbedded in the filename
##' fileName <- timeStamp("demo_load_object", "Rdata")
##'
##' # Save and reload the file
##' save(x, file = fileName)
##' rm(x)
##' y <- loadObject(fileName)
##'
##' # Note how 'x' is not in the global environment
##' ls()
##'
##' # This is the object that was in 'fileName'
##' print(y)
##'
##' # Delete the saved object
##' unlink(fileName)
##'
##' # Here's an example with two objects saved in a single file
##' a <- rnorm(10)
##' b <- rnorm(20)
##' save(a, b, file = fileName)
##'
##' # Load the results and show them
##' z <- loadObject(fileName)
##' print(z)
##'
##' unlink(fileName)

loadObject <- function(RdataFile) {

  # Load the objects into the environment of this function
  objIn <- load(RdataFile)

  # Create a list of the objects that were loaded
  makeListText <- paste("list(", paste(paste(objIn, "=", objIn), collapse = ", "),
                        ")", sep = "")
  outList <- eval(parse(text = makeListText))

  # If only one object was loaded, then return that object (not as a list)
  if (length(objIn) == 1)
    outList <- outList[[1]]

  # Return the loaded objects
  return(outList)

} # end loadObject

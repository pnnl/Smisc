##' Loads and returns the object(s) in one or more ".Rdata" files
##'
##' Loads and returns the object(s) in the ".Rdata" file.  This is useful
##' for naming the object(s) in the ".Rdata" file
##' something other than the name it was saved as.
##'
##' @export
##' @param RdataFiles A character vector containing the '.Rdata' filename(s)
##'
##' @return The object(s) contained in \code{RdataFiles}, organized into lists and named as
##' required to distinguish them completely.  See Examples.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{load}}
##'
##' @examples
##'# Create some filenames we'll use in this example
##'fileName1 <- "demo_load_object_1.Rdata"
##'fileName2 <- "demo_load_object_2.Rdata"
##'
##'# Make an example object
##'x <- data.frame(a = 1:10, b = rnorm(10))
##'
##'# Save and reload the file
##'save(x, file = fileName1)
##'rm(x)
##'y <- loadObject(fileName1)
##'
##'# Note how 'x' is not in the global environment
##'ls()
##'
##'# This is the object that was in 'fileName'
##'print(y)
##'
##'# Here's an example with two objects saved in a single file
##'a <- rnorm(10)
##'b <- rnorm(20)
##'save(a, b, file = fileName2)
##'
##'# Load the results and show them
##'z <- loadObject(fileName2)
##'print(z)
##'
##'# And here's an example with more than one file
##'both <- loadObject(c(fileName1, fileName2))
##'both
##'
##'# Delete the files
##'unlink(c(fileName1, fileName2))

loadObject <- function(RdataFiles) {

  # Verify we have a character vector
  stopifnotMsg(is.character(RdataFiles),
               "'RdataFiles' must be a character vector (or string)")

  # Verify files exist
  fe <- file.exists(RdataFiles)
  if (!all(fe)) {
    stop("These requested files do not exist: '", paste(RdataFiles[!fe], collapse = "', '"), "'")
  }

  # A function for loading and processing a single file
  loadFile <- function(singleFile) {

    # Load the objects into the environment of this function
    objIn <- load(singleFile)

    # Create a list of the objects that were loaded
    makeListText <- paste("list(", paste(paste(objIn, "=", objIn), collapse = ", "),
                          ")", sep = "")
    outList <- eval(parse(text = makeListText))

    # If only one object was loaded, then return that object (not as a list)
    if (length(objIn) == 1)
      outList <- outList[[1]]

    # Return the loaded objects
    return(outList)

  } # loadFile

  # If there's only one file
  if (length(RdataFiles) == 1) {
    objs <- loadFile(RdataFiles)
  }
  # And if there are many
  else {
    objs <- lapply(RdataFiles, loadFile)
    names(objs) <- RdataFiles
  }

  # Return the objects
  return(objs)

} # end loadObject

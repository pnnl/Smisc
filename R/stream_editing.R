##' Convenient stream editing functions: insert, substitute, or delete strings (or lines) from a file
##'
##' Convenient stream editing functions: insert, substitute, or delete strings (or lines) from a file
##'
##' The functions prefixed \code{sed} commands are the main 'worker' functions in the stream editing for
##' inserting one (or more new lines), replacing an entire line that matches a string, or substituting
##' one string with another throught the stream.  \code{streamEdit} is a general wrapper function that can
##' be used to execute any number of sequential insertion, replacement, or substitution steps.  If
##'
##' @aliases streamEdit, sed_insert, sed_replace, sed_substitute
##'
##' @export streamEdit, sed_insert, sed_replace, sed_substitute
##'
##' @usage
##' sed_insert(stream, insertion, afterLine = NULL, afterPattern = NULL)
##' sed_replace(stream, replacement, regex, ...)
##' sed_substitute(stream, pattern, replacement, ...)
##' streamEdit(commandList, stream = NULL, inFile = NULL, outFile = NULL)
##'
##' @param stream A character vector, each element typically (but not necessarily) containing the text
##' from a single line in a file.
##'
##' @param insertion A character vector that will be inserted after element \code{afterLine} into the
##' \code{stream}
##'
##' @param afterLine An integer in \code{[0:length(stream)]} that designates the element (or line number)
##' in \code{stream} after which \code{insertion} is inserted.  If \code{afterLine == 0}, the insertion
##' is placed before \code{stream}.
##'
##' @param afterPattern 
##'
##' @param replacement A character string of length 1, vectors not (yet) supported, that will be inserted to
##' to either entirely replace the line (using \code{sed_replace}) or that will be substituted (using
##' \code{sed_substitute}).  For \code{sed_replace} only, if  \code{replacement = NULL}, the entire line
##' is deleted.  For \code{sed_substitute}, setting \code{replace = ""} will remove the characters matched
##' to the \code{pattern}.
##'
##' @param pattern A character string containing the regular expression that will be used to identify
##' which the elements (or lines) in \code{stream} that will be either replaced entirely
##' (using \code{sed_replace}) or substituted (using \code{sed_substitute}).
##'
##' @param commandList A list that designates the insertion, replacement, or substitution commands that will
##' be performed on \code{stream}.  The list must have names corresponding to unique identifies of
##' "insert", "replace", and/or "substitute".  Each element in \code{commandList} must also be a list with
##' names and values that correspond to the arguments of \code{sed_insert}, \code{sed_replace},
##' and/or \code{sed_substitute}, respectively.  See examples below.
##' 
##' @param inFile A character string designating a file that is to be read (using \code{link{readLines}}
##' and will become a \code{stream},
##' where each line of the file is mapped to a single element in the character vector \code{stream}.
##' In code{streamEdit}, one (and only one) of \code{inFile} or \code{stream} must be specified.
##'
##' @param outFile A character string designating a file that the resulting, edited stream will be
##' written to using \code{\link{writeLines}}.
##'
##' @param \dots For \code{sed_replace}, these are additional named arguments to \code{\link{grep}}.
##' For \code{sed_substitute}, these are additional named arguments to \code{\link{gsub}}.
##' 
##' @return  Functions with the \code{sed} prefix return the edited character vector stream. \code{streamEdit}
##' invisibly return the edited stream, and writes the stream to a file if \code{outFile} is supplied.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{grep}}, \code{\link{gsub}}, \code{\link{readLines}}, \code{\link{writeLines}}
##'
##' @keywords misc
##'
## @examples

# Insert 'insertion' into 'stream' after line 'afterLine'
sed_insert <- function(stream, insertion, afterLine) {

  ls <- length(stream)
  
  # Append before
  if (afterLine == 0)
    outStream <- c(insertion, stream)
  
  # Append after
  else if (afterLine == ls)
    outStream <- c(stream, insertion)

  # Insert in the middle
  else if ((0 < afterLine) & (afterLine < ls)) {

    # Cut the string into two and insert
    s1 <- stream[1:after]
    s2 <- stream[(after + 1):ls]
    outStream <- c(s1, insertion, s2)

  }

  # Otherwise the range of afterLine was incorrect
  else
    stop("'afterLine' must be an integer in [0, length(stream)]")

  # Return the inserted stream
  return(outStream)
  
} # sed_insert

# Replace an entire line identified using grep with 'insertion'
sed_replace <- function(stream, pattern, replacement, ...) {

    ### TODO:  replacement = NULL removes a line
    
  # Initialize the outstream
  outStream <- stream
    
  # Find the indexes where the replacement will take place
  replaceIndexes <- grep(pattern, stream, ...)

  # If the replacement is length 1, then insert directly
  if (length(replacement) == 1) {

    if (!is.null(replacement))
      outStream[replaceIndexes] <- replacement

  # If the replacement is longer than one element
  else
   stop("'replacement' with length greater than 1 is not (yet) supported")

  return(outStream)

} # sed_replaceLine


# Substitute stringes throughout
sed_substitute <- function(stream, pattern, replacement, ...) {

   return(gsub(pattern, replacement, stream, ...))

} # sed_substitute


# Convenient wrapper for the sed functions
streamEdit <- function(commandList, stream = NULL, inFile = NULL, outFile = NULL) {

  # One or the other of 'stream' and 'inFile' must be specified
  if (sum(is.null(stream), is.null(inFile)) != 1)
    stop("One (and only one) of 'stream' or 'inFile' must be specified")

  # Verify we have a list in the commandList, and that it has acceptable values
  if (!is.list(commandList))
    stop("'commandList' must be a list.  See Details in help(streamEdit)")

  # Verify the names are valid
  if (!all(substr(tolower(names(commandList)), 1, 1) %in% c("i", "r", "s")))
    stop("Names of 'commandList' must be strings that uniquely identify 'insert', 'replace', or 'substitute'\n")

  # Verify that sub list is also a list
  if (!all(unlist(lapply(commandList, is.list))))
    stop("Each element in 'commandList' must be a list with names corresponding to the arguments of the\n"
         "corresponding command function (sed_insert(), sed_replace(), or sed_substitute())")

  # Read the file
  if (!is.null(inFile))
    stream <- readLines(inFile)

  # Assign the function names
  commands <- switch(substr(tolower(names(commandList)), 1, 1),
                     "i" = "sed_insert",
                     "r" = "sed_replace",
                     "s" = "sed_substitute")

  # Initialize the outStream with the stream
  outStream <- stream

  # Now execute the command list the stream until we're ready to return the output
  for (i in 1:length(commandList)) {

    cmdList <- commandList[[i]]
    cmd <- commands[i]
    
    # Verify the commands match functions
    if (!all(names(cmdList) %in% names(formals(cmd))))
      stop("The arguments supplied to 'commandList' for '", cmd, "' are not correct")

    # Execute the commands on the stream
    outStream <- do.call(cmd, c(list(stream = outStream), cmdList))

  }
  
  # Write the output
  if (is.null(outFile))
    writeLines(outStream, con = outFile)

  # Invisibly return the stream
  invisible(outStream)
   
} # streamEdit

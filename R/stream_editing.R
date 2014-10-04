##' Convenient sed-like stream editing functions
##'
##' Convenient sed-like stream editing functions: insert, substitute, or delete strings (or lines) from a file
##' 
##' The functions prefixed by \code{sed} are the main 'worker' functions for
##' inserting one (or more) new lines, replacing an entire line that matches a string, or substituting
##' one string with another throughout the stream.  \code{streamEdit} is a general wrapper function that can
##' be used to execute any number of sequential insertion, replacement, or substitution steps. Note that entire
##' elements (i.e. lines) in \code{stream} can be removed using the \code{sed_replace} and setting
##' \code{replacement} to \code{NULL}.
##'
##' If \code{inFile} and \code{outFile} are the same, a backup copy of \code{inFile} is made by attaching
##' "~" to the end of the filename, e.g., if the original file were \file{aFile.txt}, the backup would be
##' \file{aFile.txt~}.
##'
##' @aliases sed_insert sed_replace sed_substitute streamEdit
##'
##' @export sed_insert sed_replace sed_substitute streamEdit
##'
##' @rdname stream_editing
##' 
##' @usage
##' sed_insert(stream, after, insertion, silent = TRUE, ...)
##' sed_replace(stream, at, replacement, silent = TRUE, ...)
##' sed_substitute(stream, pattern, replacement, silent = TRUE, ...)
##' streamEdit(commandList, stream = NULL, inFile = NULL, outFile = NULL, silent = TRUE)
##'
##' @param stream A character vector, each element typically (but not necessarily) containing the text
##' from a single line in a file. 
##'
##' @param insertion A character vector that will be inserted after element \code{after} into the
##' \code{stream}
##'
##' @param after An integer or character string that designates where \code{insertion} is added to \code{stream}.
##' If \code{after} is numeric, it designates the line (or element) number in \code{stream} after which the
##' \code{insertion} will be placed. The numeric value of \code{after} must be in  \code{[0:length(stream)]}.
##' To make an insertion at the very beginning of \code{stream}, use \code{after = 0}.  If \code{after} is a
##' character string, the insertion is placed after the first element in \code{stream} that contains the string,
##' where matching is obtained using \code{\link{grep}}.
##'
##' @param at An vector of integers or a character string that designates  where \code{replacement} is placed in
##' \code{stream}.  If \code{at} is numeric, it designates the lines (or elements) in \code{stream}
##' that will be replaced with \code{replacement}.  The numeric value(s) of \code{at} must be in
##' \code{[1:length(stream)]}. If \code{at} is a
##' character string, the lines in \code{stream} that contain the string \code{at} are replaced with
##' \code{replacement}.
##' 
##' @param replacement A character string of length 1, vectors not (yet) supported, that will be inserted to
##' to either entirely replace the line (using \code{sed_replace}) or that will be substituted (using
##' \code{sed_substitute}).  For \code{sed_replace} only, if  \code{replacement = NULL}, the entire line
##' is deleted.  For \code{sed_substitute}, setting \code{replace = ""} will remove the characters matched
##' to \code{pattern}.
##'
##' @param pattern A character string containing the regular expression that will be used to identify
##' which the elements (or lines) in \code{stream} that will be substituted using \code{sed_substitute}.
##'
##' @param commandList A list that designates the insertion, replacement, or substitution commands that will
##' be performed on \code{stream}.  The list must have names corresponding to unique abbreviations of
##' "insert", "replace", and/or "substitute".  Each element in \code{commandList} must also be a list with
##' names and values that correspond to the arguments of \code{sed_insert}, \code{sed_replace},
##' and/or \code{sed_substitute}, respectively.  See example below.
##' 
##' @param inFile A character string designating a file that is to be read (using \code{link{readLines}}
##' and will become a \code{stream},
##' where each line of the file is mapped to a single element in the character vector \code{stream}.
##' In code{streamEdit}, one (and only one) of \code{inFile} or \code{stream} must be specified.
##'
##' @param outFile A character string designating a file that the resulting, edited stream will be
##' written to using \code{\link{writeLines}}.
##'
##' @param silent A logical that, when \code{TRUE}, instructs the \code{sed} functions to simply return the
##' \code{stream} if the \code{pattern} is not matched in the \code{stream}.  If \code{silent = FALSE}, a warning
##' is issued when a match for \code{pattern} is not found in the \code{stream}.
##'
##' @param \dots For \code{sed_insert} and \code{sed_replace}, these are additional named arguments to
##' \code{\link{grep}}, which are applicable if \code{after} or \code{at} is a character string.
##' For \code{sed_substitute}, these are additional named arguments to \code{\link{grepl}} and \code{\link{gsub}}.
##' 
##' @return  Functions with the \code{sed} prefix return the edited character vector stream. \code{streamEdit}
##' invisibly returns the edited stream, and writes the stream to a file if \code{outFile} is supplied.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{grep}}, \code{\link{gsub}}, \code{\link{readLines}}, \code{\link{writeLines}}
##'
##' @keywords misc
##'
##' @examples
##'# Let's create an example stream we can edit
##'cat("Here's a line\n",
##'    "Here's a line we'll delete\n",
##'    "Line after which we'll insert a string\n",
##'    "Filler line\n",
##'    "A line after which we'll insert another string\n",
##'    "A line where we'll make a substitution\n",
##'    "A line where we'll delete 'this'\n",
##'    "A line we'll delete\n",
##'    "A line we'll entirely replace\n",
##'    "The last line\n",
##'    sep = "", file = "tmpTest_streamEdit.txt")
##'
##'# Show the file
##'more("tmpTest_streamEdit.txt")
##'
##'# Read the file into a 'stream'
##'stream <- readLines("tmpTest_streamEdit.txt")
##'
##'# Insert a string using line numbers
##'stream <- sed_insert(stream, after = 3, "Here's the first insertion")
##'stream
##'
##'# Insert a stream by searching
##'stream <- sed_insert(stream, c("Here's the second insertion", "Another line of the second insertion"),
##'                     after = "insert another")
##'stream
##'
##'# Here's a deletion of lines 1 and 2 using line numbers
##'stream <- sed_replace(stream, 1:2, NULL)
##'stream
##'
##'# Here's a line deletion using a search string
##'stream <- sed_replace(stream, "A line we'll delete", NULL)
##'stream
##'
##'# Here's a line replacement
##'stream <- sed_replace(stream, "entirely", "A replacement for the line")
##'stream
##'
##'# Here's a deletion within the line
##'stream <- sed_substitute(stream, " 'this'", "")
##'stream
##'
##'# Here's a substitution of text
##'stream <- sed_substitute(stream, "substitution", "correction")
##'stream
##'
##'# Now let's apply the same changes all at once using streamEdit()
##'stream1 <- streamEdit(list(i = list(after = 3, insertion = "Here's the first insertion"),
##'                           i = list(after = "insert another",
##'                                    insertion = c("Here's the second insertion",
##'                                                  "Another line of the second insertion")),
##'                           r = list(at = 1:2, replacement = NULL),
##'                           r = list(at = "A line we'll delete", replacement = NULL),
##'                           r = list(at = "entirely", replacement = "A replacement for the line"),
##'                           s = list(pattern = " 'this'", replacement = ""),
##'                           s = list(pattern = "substitution", replacement = "correction")),
##'                      inFile = "tmpTest_streamEdit.txt")
##'
##'# Compare the results
##'identical(stream, stream1)
##'
##'# Remove the file
##'unlink("tmpTest_streamEdit.txt")
                               
# Insert 'insertion' into 'stream' after line 'after'
sed_insert <- function(stream, after, insertion, silent = TRUE, ...) {

  # Basic checks
  stopifnot(is.character(stream),
            is.character(insertion),
            is.logical(silent),
            length(silent) == 1)
    
  # Verify 'after' is atomic and of the correct type
  if (!((length(after) == 1) & (is.character(after) | is.numeric(after))))
    stop("'after' must be an integer in [0, length(stream)] or a single character string")

  # Get the length of the stream
  ls <- length(stream)

  # If it's character, figure out the first line after which the insertion should take place
  if (is.character(after)) {

    pattern <- after
    after <- grep(after, stream, ...)[1]

    # If match was not found, just return the string
    if (!length(after)) {

      if (!silent) 
        warning("The pattern '", pattern, "' was not found in 'stream'")
      
      return(stream)

    }

  }
  
  # Otherwise, verify it's in the acceptable range
  else if (!(after %in% 0:ls))
    stop("'after' must be an integer in [0, length(stream)] or a single character string")    
  
  # Append before
  if (after == 0)
    outStream <- c(insertion, stream)
    
  # Append after
  else if (after == ls)
    outStream <- c(stream, insertion)
  
  # Insert in the middle
  else {
  
    # Cut the string in two and insert
    s1 <- stream[1:after]
    s2 <- stream[(after + 1):ls]
    outStream <- c(s1, insertion, s2)
  
  }
 
  # Return the inserted stream
  return(outStream)
  
} # sed_insert

# Replace an entire line identified using grep with 'insertion'
sed_replace <- function(stream, at, replacement, silent = TRUE, ...) {

  # Basic checks
  stopifnot(is.character(stream),
            is.logical(silent),
            length(silent) == 1)

  # Initialize the outstream
  outStream <- stream

  # If it's character, figure out lines where replacement should take place
  if (is.character(at)) {

    pattern <- at
    at <- grep(at, stream, ...)

    # If match was not found, just return the string
    if (!length(at)) {

      if (!silent) 
        warning("The pattern '", pattern, "' was not found in 'stream'")
      
      return(stream)

    }

  }

  # Otherwise, verify it's in the acceptable range
  else if (!all(at %in% 1:length(stream)))
    stop("'at' must be an integer in [1, length(stream)] or a single character string")    

     
  # If a string was provided
  if (!is.null(replacement)) {

    # Verify we have a character string
    stopifnot(is.character(replacement))
    
    # If the replacement is length 1, then insert directly      
    if (length(replacement) == 1) 
      outStream[at] <- replacement
  
    # If the replacement is longer than one element
    else
      stop("'replacement' with length greater than 1 is not (yet) supported")
  }
  # If NULL was provided
  else 
    outStream <- outStream[-at]
 
  # Return the edited stream
  return(outStream)

} # sed_replace


# Substitute strings throughout the stream
sed_substitute <- function(stream, pattern, replacement, silent = TRUE, ...) {

  # Basic checks
  stopifnot(is.character(stream),
            is.character(pattern),
            length(pattern) == 1,
            is.character(replacement),
            length(replacement) == 1,
            is.logical(silent),
            length(silent) == 1)

  # If the pattern isn't present
  if (!any(grepl(pattern, stream, ...))) {
      
    if (!silent) 
      warning("The pattern '", pattern, "' was not found in 'stream'")
      
    return(stream)

  }
  else
    return(gsub(pattern, replacement, stream, ...))

} # sed_substitute


# Convenient wrapper for the sed functions
streamEdit <- function(commandList, stream = NULL, inFile = NULL, outFile = NULL) {

  # One or the other of 'stream' and 'inFile' must be specified
  if (sum(is.null(stream), is.null(inFile)) != 1)
    stop("One (and only one) of 'stream' or 'inFile' must be specified")

  # Verify we have a list in the commandList, and that it has acceptable values
  if (!is.list(commandList))
    stop("'commandList' must be a list. See help(streamEdit)")

  # Verify the names are valid
  if (!all(substr(tolower(names(commandList)), 1, 1) %in% c("i", "r", "s")))
    stop("Names of 'commandList' must be strings that uniquely identify 'insert', 'replace', or 'substitute'\n")

  # Verify that sub list is also a list
  if (!all(unlist(lapply(commandList, is.list))))
    stop("Each element in 'commandList' must be a list with names corresponding to the arguments of the\n",
         "corresponding command function (sed_insert(), sed_replace(), or sed_substitute())")

  # Read the file
  if (!is.null(inFile))
    stream <- readLines(inFile)

  # Assign the function names
  commands <- unlist(lapply(substr(tolower(names(commandList)), 1, 1),
                            function(x) switch(x,
                                               "i" = "sed_insert",
                                               "r" = "sed_replace",
                                               "s" = "sed_substitute")))

  # Initialize the outStream with the stream
  outStream <- stream

  # Now execute the command list the stream until we're ready to return the output
  for (i in 1:length(commandList)) {

    cmdList <- commandList[[i]]
    cmd <- commands[i]

    # Execute the commands on the stream
    outStream <- do.call(cmd, c(list(stream = outStream), cmdList))

  }

  # Write the output
  if (!is.null(outFile)) {

    # Make a backup copy of the inFile if the outFile is the same
    if (!is.null(inFile)) {
      if (inFile == outFile) 
        file.copy(inFile, paste(inFile, "~", sep = ""))
    }

    writeLines(outStream, con = outFile)

  }

  # Invisibly return the stream
  invisible(outStream)
   
} # streamEdit

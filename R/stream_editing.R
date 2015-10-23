##' Convenient sed-like stream editing functions
##'
##' Convenient sed-like stream editing functions to insert, substitute, delete, or comment strings (or lines)
##' in a file.  The functions prefixed by \code{sed} are the main 'worker' functions for
##' inserting one (or more) new lines, replacing an entire line that matches a string, substituting
##' one string with another, or commenting/uncommenting lines throughout the stream.
##' \code{streamEdit} is a general wrapper function that can
##' be used to execute any number of sequential insertion, replacement, substitution, or commenting steps.
##'
##' Note that entire
##' elements (i.e. lines) in \code{stream} can be removed using the \code{sed_replace} and setting
##' \code{replacement} to \code{NULL}.
##' 
##' If \code{inFile} and \code{outFile} are the same, a backup copy of \code{inFile} is made by attaching
##' "~" to the end of the filename, e.g., if the original file were \file{aFile.txt}, the backup would be
##' \file{aFile.txt~}.
##'
##' The value of \code{silent} in \code{streamEdit} is passed to the worker functions (\code{sed_insert},
##' \code{sed_replace}, and \code{sed_substitute}) unless the \code{silent} argument is specified for a
##' command in \code{commandList}, in which case, for that particular command, the locally supplied value
##' of \code{silent} takes precedence.
##'
##' @aliases sed_insert sed_replace sed_substitute sed_comment streamEdit
##'
##' @export sed_insert sed_replace sed_substitute sed_comment streamEdit
##'
##' @rdname stream_editing
##' 
##' @usage
##' sed_insert(stream, after, insertion, silent = TRUE, ...)
##' sed_replace(stream, at, replacement, silent = TRUE, ...)
##' sed_substitute(stream, pattern, replacement, every = TRUE, silent = TRUE, ...)
##' sed_comment(stream, at, add = TRUE, type = c("R", "C", "java", "html", "tex"),
##'             silent = TRUE, ...)
##' streamEdit(commandList, stream = NULL, inFile = NULL, outFile = NULL, silent = TRUE)
##'
##' @param stream A character vector, each element typically (but not necessarily) containing the text
##' from a single line in a file. 
##'
##' @param insertion A character vector that will be inserted into the stream after element \code{after}.
##' Each element in the vector could correspond to a separate line.
##'
##' @param after An integer or character string that designates where \code{insertion} is added to \code{stream}.
##' If \code{after} is numeric, it designates the line (or element) number in \code{stream} after which the
##' \code{insertion} will be placed. The numeric value of \code{after} must be in \code{[0:length(stream)]}.
##' To make an insertion at the very beginning of \code{stream}, use \code{after = 0}.  If \code{after} is a
##' character string, the insertion is placed after the first element in \code{stream} that contains the string,
##' where matching is obtained using \code{\link{grep}}.
##'
##' @param at An vector of integers or a character string that designates where \code{replacement}
##' is placed in \code{stream}.  If \code{at} is numeric, it designates the lines (or elements) in \code{stream}
##' that will be replaced with \code{replacement}.  The numeric value(s) of \code{at} must be in
##' \code{[1:length(stream)]}. If \code{at} is a
##' character string, the lines in \code{stream} that contain the string \code{at} are replaced with
##' \code{replacement}.  For \code{sed_comment}, the corresponding lines are commented or uncommented.
##' 
##' @param replacement A character string of length 1 (vectors not supported), that will be inserted 
##' to either entirely replace the line (using \code{sed_replace}) or that will be substituted (using
##' \code{sed_substitute}).  For \code{sed_replace} only, if  \code{replacement = NULL}, the entire line
##' is deleted.  For \code{sed_substitute}, setting \code{replace = ""} will remove the characters matched
##' to \code{pattern}.
##'
##' @param pattern A character string containing the regular expression that will be used to identify
##' which the elements (or lines) in \code{stream} that will be substituted using \code{sed_substitute}.
##'
##' @param every A logical indicating whether every instance of \code{pattern} in a single line should be
##' substituted with \code{replacement}, in which case \code{\link{gsub}} is used.
##' If \code{every = FALSE}, only the first instance of \code{pattern} is substituted, in which case
##' \code{\link{sub}} is used.
##'
##' @param add A logical indicating whether comments are added to a single
##' line (\code{TRUE}), or removed (\code{FALSE}). For \code{sed_comment}, note that comment symbols are
##' added/removed to/from a single line at a time, i.e. not across multiple lines.  When comments are added,
##' the comment symbols are placed at the beginning and end of the selected lines.  When comments are removed,
##' the first instances of the beginning and ending symbols are removed from the selected lines.
##' 
##' @param type A character string indicating the type of comment characters that will be added (or removed).
##' Alternatively, if a character vector of length 2 is provided, \code{type[1]} designates the beginning
##' comment character and \code{type[2]} designates the ending comment character.
##'
##' @param commandList A list that designates the insertion, replacement, substitution, or commenting commands
##' that will
##' be performed on \code{stream}.  The list must have names corresponding to unique abbreviations of
##' "insert", "replace", "substitute", and/or "comment".  Each element in \code{commandList} must also be
##' a list with
##' names and values that correspond to the arguments of \code{sed_insert}, \code{sed_replace},
##' \code{sed_substitute}, and/or \code{sed_comment} respectively.  See example below.
##' 
##' @param inFile A character string designating a file that is to be read (using \code{link{readLines}}
##' and will become a \code{stream},
##' where each line of the file is mapped to a single element in the character vector \code{stream}.
##' In \code{streamEdit}, one (and only one) of \code{inFile} or \code{stream} must be specified.
##'
##' @param outFile A character string designating a file that the resulting, edited stream will be
##' written to using \code{\link{writeLines}}.
##'
##' @param silent A logical that, when \code{TRUE}, instructs the \code{sed} functions to simply return the
##' \code{stream} if the \code{pattern} is not matched in the \code{stream}.  If \code{silent = FALSE}, a message
##' is printed when a match for \code{pattern} is not found in the \code{stream}.
##'
##' @param \dots For \code{sed_insert}, \code{sed_replace}, and \code{sed_comment} these are additional
##' named arguments to
##' \code{\link{grep}}, which are applicable if \code{after} or \code{at} is a character string.
##' For \code{sed_substitute}, these are additional named arguments to \code{\link{grepl}} and
##' \code{\link{gsub}} or \code{\link{sub}}.
##' 
##' @return
##' \itemize{
##' \item Functions with the \code{sed} prefix return the edited character vector stream.
##' \item \code{streamEdit} invisibly returns the edited stream, and writes the stream to a file if
##' \code{outFile} is supplied.
##' }
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{grep}}, \code{\link{sub}}, \code{\link{gsub}}, \code{\link{readLines}},
##' \code{\link{writeLines}}
##'
##' @keywords misc
##'
##' @examples
##'################################################################################
##'# Let's create an example stream we can edit
##'################################################################################
##' 
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
##'print(stream <- sed_insert(stream, c("Here's the second insertion", "Another line of the second insertion"),
##'                           after = "insert another"))
##'
##'# Here's a deletion of lines 1 and 2 using line numbers
##'print(stream <- sed_replace(stream, 1:2, NULL))
##'
##'# Here's a line deletion using a search string
##'print(stream <- sed_replace(stream, "A line we'll delete", NULL))
##'
##'# Here's a line replacement
##'print(stream <- sed_replace(stream, "entirely", "A replacement for the line"))
##'
##'# Here's a deletion within the line
##'print(stream <- sed_substitute(stream, " 'this'", ""))
##' 
##'# Here's a substitution of text
##'print(stream <- sed_substitute(stream, "substitution", "correction"))
##'
##'# And commenting the last line
##'print(stream <- sed_comment(stream, "last", type = "html"))
##'
##'# Uncommenting it
##'print(stream <- sed_comment(stream, "last", add = FALSE, type = "html"))
##'
##'# Custom commenting on the last line
##'print(stream <- sed_comment(stream, "last", type = c("&& ", " *;")))
##'
##'################################################################################
##'# Now let's apply the same changes all at once using streamEdit()
##'################################################################################
##' 
##'stream1 <- streamEdit(list(i = list(after = 3, insertion = "Here's the first insertion"),
##'                           i = list(after = "insert another",
##'                                    insertion = c("Here's the second insertion",
##'                                                  "Another line of the second insertion")),
##'                           r = list(at = 1:2, replacement = NULL, silent = FALSE),
##'                           r = list(at = "A line we'll delete", replacement = NULL),
##'                           r = list(at = "entirely", replacement = "A replacement for the line"),
##'                           s = list(pattern = " 'this'", replacement = "", silent = FALSE),
##'                           s = list(pattern = "substitution", replacement = "correction"),
##'                           c = list(at = "last", type = "html"),
##'                           c = list(at = "last", add = FALSE, type = "html"),
##'                           c = list(at = "last", type = c("&& ", " *;"))),
##'                       inFile = "tmpTest_streamEdit.txt")
##'
##'# Compare the results
##'identical(stream, stream1)
##'
##'# Remove the file
##'unlink("tmpTest_streamEdit.txt")
##'
##'################################################################################
##'# Some additional examples for sed_comment
##'################################################################################
##'
##'aStream <- c("Here's a line to comment",
##'             "# A line to uncomment",
##'             "  <!-- Another commented line --> ",
##'             "And some comments * embedded in the line ;")
##'
##'# Comment the first line in C style
##'sed_comment(aStream, "to comment", type = "C")
##'
##'# Comment the first line with a custom style
##'print(a <- sed_comment(aStream, "to comment", type = c("&&", "##")))
##'
##'# Remove the R comment from the 2nd line
##'print(a <- sed_comment(a, "to un", add = FALSE, type = "R"))
##'
##'# Remove the html comments
##'print(a <- sed_comment(a, "Another", add = FALSE, type = "html"))
##'
##'# Remove the SAS comments
##'sed_comment(a, "embedded", add = FALSE, type = "SAS")
##'
##'# Comment every line Java style
##'print(b <- sed_comment(aStream, "comment", type = "Java"))
##'
##'# Remove the Java comments from the second and fourth lines
##'sed_comment(b, c(2, 4), add = FALSE, type = "Java")

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
        cat("The pattern '", pattern, "' was not found in 'stream'\n", sep = "")
      
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
        cat("The pattern '", pattern, "' was not found in 'stream'\n", sep = "")
      
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
sed_substitute <- function(stream, pattern, replacement, every = TRUE, silent = TRUE, ...) {

  # Basic checks
  stopifnot(is.character(stream),
            is.character(pattern),
            length(pattern) == 1,
            is.character(replacement),
            length(replacement) == 1,
            is.logical(every),
            length(every) == 1,
            is.logical(silent),
            length(silent) == 1)

  # If the pattern isn't present
  if (!any(grepl(pattern, stream, ...))) {
      
    if (!silent) 
      cat("The pattern '", pattern, "' was not found in 'stream'\n", sep = "")
      
    return(stream)

  }
  else {
    if (every)
      return(gsub(pattern, replacement, stream, ...))
    else
      return(sub(pattern, replacement, stream, ...))
  }

} # sed_substitute


# Comment or uncomment an entire line
sed_comment <- function(stream, at, add = TRUE,
                        type = c("R", "C", "java", "html", "tex", "SAS"),
                        silent = TRUE, ...) {

  # Basic checks
  stopifnot(is.character(stream),
            is.logical(add),
            length(add) == 1,
            is.character(type),
            is.logical(silent),
            length(silent) == 1)

  # If it's character, figure out lines where commenting should take place
  if (is.character(at)) {

    pattern <- at
    at <- grep(at, stream, ...)

    # If match was not found, just return the string
    if (!length(at)) {

      if (!silent) 
        cat("The pattern '", pattern, "' was not found in 'stream'\n", sep = "")
      
      return(stream)

    }

  }

  # Otherwise, verify it's in the acceptable range
  else if (!all(at %in% 1:length(stream)))
    stop("'at' must be an integer in [1, length(stream)] or a single character string")    

  # Customized comment characters
  if (length(type) == 2) {
      
    beginComment <- type[1]
    endComment <- type[2]
    
  }
  else {

    # Match the type argument to the acceptable values
    type <- match.arg(type)

    # Figure out which comment characters will be applied to beginning and end of lines
    beginComment <- switch(type,
                           "R" = "#",
                           "C" = "//",
                           "Java" = "//",
                           "html" = "<!--",
                           "tex" = "%",
                           "SAS" = "*")
  
    endComment <- switch(type,
                         "R" = "",
                         "C" = "",
                         "Java" = "",
                         "html" = "-->",
                         "tex" = "",
                         "SAS" = ";")
  }

  # Initialize the outstream
  outStream <- stream

  # Comment the appropriate lines
  if (add) {
    outStream[at] <- paste(beginComment, stream[at], endComment, sep = "")
  }
  # Remove the comments from lines if comments exist
  else {
      
    # Remove the first instance of the beginning comment character
    str <- sub(beginComment, "", stream[at], fixed = TRUE) 

    # Remove the first instance of the ending comment symbol
    if (nchar(endComment)) {
      str <- sub(endComment, "", str, fixed = TRUE)
    }

    # Insert into the outStream
    outStream[at] <- str
  
  } # Removing comments
     
  # Return the edited stream
  return(outStream)

} # sed_comment


# Convenient wrapper for the sed functions
streamEdit <- function(commandList, stream = NULL, inFile = NULL, outFile = NULL, silent = TRUE) {

  # One or the other of 'stream' and 'inFile' must be specified
  if (sum(is.null(stream), is.null(inFile)) != 1)
    stop("One (and only one) of 'stream' or 'inFile' must be specified")

  # Verify we have a list in the commandList, and that it has acceptable values
  if (!is.list(commandList))
    stop("'commandList' must be a list. See help(streamEdit)")

  # Verify the names are valid
  if (!all(substr(tolower(names(commandList)), 1, 1) %in% c("i", "r", "s", "c")))
    stop("Names of 'commandList' must be strings that uniquely identify 'insert', 'replace',\n",
         "'substitute', or 'comment'")

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
                                               "s" = "sed_substitute",
                                               "c" = "sed_comment")))

  # Initialize the outStream with the stream
  outStream <- stream

  # Now execute the command list the stream until we're ready to return the output
  for (i in 1:length(commandList)) {

    cmdList <- commandList[[i]]
    cmd <- commands[i]

    # If a value for silent is provided in the command list, leave it alone.  Otherwise,
    # set it to the overall value of silent when streamEdit() was called
    if (!("silent" %in% names(cmdList)))
      cmdList <- c(cmdList, list(silent = silent))

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

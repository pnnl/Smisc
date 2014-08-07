##' Indicates whether a vector of email addreses may be valid
##'
##' Indicates whether a vector of email addreses may be valid
##'
##' Checks the following conditions:
##'
##' 1: Only 1 @@ allowed
##'
##' 2: Should have text on both sides of the @@
##'
##' 3: Text on right hand side (RHS) of @@ should have at least one dot
##'
##' 4: RHS text should not end with a dot
##'
##' 5: The suffix of the RHS should have 2 or 3 characters (e.g. 'com', 'net',
##' 'uk')
##'
##' 6: RHS should not begin with a dot
##'
##' 7: No illegal characters--however, it does't check for a backslash
##'
##' @export
##'
##' @param vec Character vector of email addresses
##' 
##' @return Logical vector which is \code{TRUE} when the email address is
##' valid.
##' 
##' @author Landon Sego
##' 
##' @references Based in part on
##' \url{http://groups.google.com/group/eaut/web/rules-for-valid-email-addresses}
##' 
##' @keywords misc
##' @examples
##'
##' validEmailAddress(c(NA, "@@","this@@that","this@@that@@that","this@@that.com.ar",
##'                       "@@this.com","that@@","this@@that.","this@@.that","this@@.that.com",
##'                        "this@@that.com","this(@@that.com","this@@that .com"))

validEmailAddress <- function(vec) {

  if (!is.character(vec))
    stop("'vec' must be a character vector\n")

  # Get the names for later
  n.vec <- names(vec)

  # If NA, supplant with one that will work and then substitue NA at the end
  are.NA <- is.na(vec)
  vec[are.NA] <- "email@fake.com"

  # (This only.1.ampersand test is necessary since the strsplit condition
  #  will not recognize the situation where an email address ends with a @)
  only.1.ampersand <- unlist(lapply(gregexpr("@", vec), function(x) length(x[x != -1]))) == 1

  # After the split, need exactly two strings and each string should have positive length (nchar > 0)
  text.on.both.sides <- unlist(lapply(strsplit(vec, "\\@"), function(x) (length(x) == 2) & (all(nchar(x) > 0))))

  # Get the strings to the right of the @
  rhs <- grabLast(vec, "@") # unlist(lapply(strsplit(vec, "\\@"), function(x) x[2]))

  # Verify the rhs has at least one 'dot'
  at.least.one.dot <- regexpr("\\.", vec) != -1

  # Verify the rhs does not end with a dot
  rhs.ends.with.text <- nchar(stripExtension(rhs)) > 0

  # Verify the suffixes have 2 or 3 characters
  suffix.length <- nchar(getExtension(rhs)) %in% 2:3

  # Verify that rhs doesn't begin with a dot
  no.first.dot <- substr(rhs, 1, 1) != "."

  # Verify no illegal characters (don't know how to search for the backslash...)
  # (Can add or delete characters from the illegal vector as desired--rest of code will still work

  # Based in part on
  # http://groups.google.com/group/eaut/web/rules-for-valid-email-addresses
  illegal <- c(" ", '"', "(", ")", ",", ":", ";", "<", ">", "[", "]", "`", "~")
#    vec <- c(illegal, "ok@email.com")  # For testing
  names(illegal) <- labels <- letters[1:length(illegal)]

  # Test the legality of vec for each illegal character.  Write the results to legal.a, legal.b, etc...
  for (i in 1:length(illegal))
    assign(paste("legal", labels[i], sep="."), regexpr(illegal[i], vec, fixed=TRUE) == -1)

  # for Testing
#    for (i in labels)
#      pvar(get(paste("legal", i, sep=".")))

  # Combine all the legal vectors into a matrix, each column corresponding to the 'legality' of vec for a specific illegal character
  legal.mat <- matrix(eval(parse(text=paste("c(", paste(paste("legal", labels, sep="."), collapse=","), ")", sep=""))),
                      ncol=length(labels), dimnames=list(1:length(vec), labels))

  # Summarize accross the matrix
  legal <- apply(legal.mat, 1, all)

  # Return an indicator vector of those emails which satisfy all the conditions
  final <- only.1.ampersand & text.on.both.sides & at.least.one.dot & rhs.ends.with.text & suffix.length & no.first.dot & legal

  # Reinsert the NA's
  final[are.NA] <- NA
  names(final) <- n.vec

  return(final)

} # validEmailAddress

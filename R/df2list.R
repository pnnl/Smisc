##' Row-wise conversion of a data frame to a list
##'
##' Convert a data frame to a list, where each element of the output list
##' consists of a named list (or a named vector) containing a single row of the
##' data frame.
##'
##' @export
##' @param df A data frame
##'
##' @param out.type Character string uniquely identifying 'list', 'data.frame',
##' or 'vector'.  If 'list', then each row of the data frame is output as a
##' list.  If 'data.frame', then each row of the data frame is output as a
##' 1-row data frame.  If 'vector', then each row of the data frame is output
##' as a named vector. However, for 'vector,' each column of the data set must
##' be the same type.  Defaults to 'list'.
##'
##' @return A list where each element consists of a named list, single-row data
##' frame, or a named vector, containing a single row of the original data
##' frame.
##'
##' @author Landon Sego
##'
##' @seealso \code{\link{list2df}}, \code{\link{as.list}}
##'
##' @keywords misc
##'
##' @examples
##' d <- data.frame(a = 1:3, b = letters[1:3])
##' df2list(d)
##'
##' d1 <- data.frame(a = 1:3, b = 7:9)
##' df2list(d1, out.type = "v")
##'

df2list <- function(df, out.type = c("list", "data.frame", "vector")) {

  if (!is.data.frame(df))
    stop("'", deparse(substitute(df)), "' is not a data frame\n")

  out.type <- match.arg(out.type)

  # Create the target list
  out <- vector(mode="list", length = NROW(df))
  names(out) <- rownames(df)

  if (out.type == "list") {

    for (i in 1:NROW(df))
      out[[i]] <- as.list(df[i,])

  }
  else if (out.type == "data.frame") {

    for (i in 1:NROW(df))
      out[[i]] <- df[i,]

  }
  else {

    # Verify that each element of the data frame is the same type
    if (length(table(unlist(lapply(df, class)))) != 1)
      stop('To use `out.type = "vector"`, all columns of the data frame must be of the same type')

    for (i in 1:NROW(df))
      out[[i]] <- unlist(df[i,])

  }

  return(out)

} # df2list

# Convert a data frame into a list, where each element of the list consists of a named list containing a row of the data frame

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

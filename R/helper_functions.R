################################################################################
# Non-exported helper functions that are used internally in Smisc
################################################################################

# A helper function for stripping attributes when printing
printWithoutAttributes <- function(x, ...) {
  y <- x
  attributes(y) <- list(names = names(x))
  print(y, ...)
}

# Create the union of the defaultArgs and the supplied args, ..., but supplied args get preference
# if there are two of the same name
blendArgs <- function(defaultArgs, ...) {

  # Find names in ... that exist in defaultArgs that will replace those in defaultArgs
  suppliedArgs <- list(...)
  toReplace <- intersect(names(defaultArgs), names(suppliedArgs))

  # Remove args from defaultArgs that are provided to ...
  if (length(toReplace)) {
    defaultArgs <- defaultArgs[-which(names(defaultArgs) %in% toReplace)]
  }

  # Blend the two sets of arguments
  return(c(defaultArgs, suppliedArgs))
   
}

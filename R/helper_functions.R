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

  # Supplied extra arguments, converted to a list
  suppliedArgs <- list(...)

  # Add in defaultArgs not already in suppliedArgs
  return(c(suppliedArgs, defaultArgs[setdiff(names(defaultArgs), names(suppliedArgs))]))
  
}

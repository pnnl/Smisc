# Source all the .R (or .r) files in a directory.

sourceDir <- function(directory, recursive = FALSE, ...) {

 x <- dir(directory, full.names = TRUE, recursive = recursive)
 files <- x[toupper(substr(x, nchar(x) - 1, nchar(x))) == ".R"]

 if (length(files)) {

   for (i in files) {
  
     if (class(err <- try(source(i, ...), silent = TRUE)) != "try-error")
       cat("Sourcing '", i, "'\n", sep = "")
     else
       cat("File '", i, "' failed to source correctly:\n",
           err, sep = "")
     
   } # for (i in files)
   
 } # if length(files)

 else
   cat("There are no files with '.R' or '.r' extensions in", directory, "\n")

 invisible(files)
 
} # end sourceDir()

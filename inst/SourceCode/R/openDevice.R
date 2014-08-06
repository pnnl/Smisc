# Open a graphics device based on the extension of the filename that is provided
openDevice <- function(fileName, ...) {

  fileName <- path.expand(fileName)
  
  ext <- tolower(getExtension(fileName))

  supported.extensions <- c("ps","pdf","jpg","tif","png","bmp")

  if (!(ext %in% supported.extensions))
    stop("Filename must have one of the following extensions: '",
         paste(supported.extensions, collapse="', '"), "'.\n")

  useCairo <- FALSE
  
  # If we're on olympus or any of the pic nodes, use Cairo
  if (.Platform$OS.type == "unix") {

    usr.host <- system("/bin/hostname -s", intern = TRUE)

    if (gsub("[0-9]", "", usr.host) %in% c("olympus", "olympus-e", "node", "gpu", "fat", "short")){

      if (!(ext %in% c("ps", "pdf")))
        useCairo <- TRUE

    }
      
  }


  # Without Cairo
  if (!useCairo) 
    fname <- switch(ext,
                    ps = "postscript",
                    pdf = "pdf",
                    jpg = "jpeg",
                    tif = "tiff",
                    png = "png",
                    bmp = "bmp")

  # With Cairo
  else {

    cat("Message from openDevice():  'useCairo' is being set to TRUE because it appears you ",
        "are running on PIC\n", sep = "")

    if (ext == "bmp")
     stop("bmp is not a supported format for Cairo")

    stopifnot(require(Cairo))

    fname <- switch(ext,
                    jpg = "CairoJPEG",
                    tif = "CairoTIFF",
                    png = "CairoPNG")
  }   
  
  # Get the argument names of the function that will be called
  argNames <- names(formals(fname))

  # Add the appropriate first argument name to our list
  vList <- list(fileName, ...)
  names(vList)[1] <- argNames[1]

  # In the call, drop any arguments from our list that are not in the arguments
  # of the desired function and open the device
  do.call(fname, vList[intersect(names(vList), argNames)])

  # invisibly return the filename
  invisible(fileName)

} # openDevice
  

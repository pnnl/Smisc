load.object <- function(Rdata.file) {

  obj.in <- load(Rdata.file)

  if (length(obj.in) > 1)
    warning(deparse(substitute(Rdata.file)), " contains more than one object. ",
            "Only object '", obj.in[1], "' will be returned.\n", sep="")

  return(get(obj.in))
  
} # end load.object

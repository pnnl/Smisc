# Pads a character or numeric numbers with leading 0's

padZero <- function(vec, num=NULL, side=c("left", "right")) {

  side <- match.arg(side)

  c.vec <- as.character(vec)
  nchar.vec <- nchar(c.vec)
  m.nchar.vec <- max(nchar.vec)

  if (side == "left") {

    if (is.null(num))
      num <- m.nchar.vec
    else if (num < m.nchar.vec)
      warning("num = ", num, " is less than the largest number of characters, ", m.nchar.vec, ".\n")
  
    # Create the 0 vector
    zV <- tapply(nchar.vec, 1:length(nchar.vec), function(x) paste(rep(0, max(0, num - x)), collapse=""))
  
    out <- paste(zV, c.vec, sep="")
    
  } # if padding on the left

  # padding on the right
  else {

    if (!is.numeric(vec))
      stop("'vec' should be numeric when side='right'\n")

    # Grab the decimal portion of the vector and identify the maximum length
    m.dec.length <- max(nchar(getExtension(c.vec)))

    if (is.null(num))
      num <- m.dec.length
    else if (num < m.dec.length)
      warning("num = ", num, " is less than the largest number of decimals, ", m.dec.length,
              ", so rounding will occur.\n")

    out <- sprintf(paste("%.", num, "f", sep=""), vec)
    
  } # else

  return(out)

} # padZero

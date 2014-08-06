# Assess whether data frames are equivalent
# Uses the convention that NA==NA returns TRUE
# 
# Tests for exact equivalency for numeric values (is doesn't try to deal with
# machine error precision)

dframeEquiv <- function(d1, d2, maxAbsError=1e-12, maxRelError=1e-14, verbose=TRUE) {

  d1.d2 <- paste("'", deparse(substitute(d1)), "' and '",
                 deparse(substitute(d2)), "'", sep="")

  # Initialize outputs
  frac.equiv <- loc.inequiv <- equiv.mat <- msg <- NULL
  
  if (!((is.data.frame(d1) | is.matrix(d1)) &
        (is.data.frame(d2) | is.matrix(d2))))
    stop(d1.d2, " must both be data frames or matrices.")

  keep.checking <- TRUE
  
  # Check for number of rows
  if (NROW(d1) != NROW(d2)) {
    msg <- paste(d1.d2, "have a different number of rows.")
    keep.checking <- FALSE
  }

  # Check for number of columns
  if (NCOL(d1) != NCOL(d2)) {
    msg <- c(msg, paste(d1.d2, "have a different number of columns."))
    keep.checking <- FALSE
  }

  if (keep.checking) { 

    # Check colnames
    colStatus <- sum(c(is.null(colnames(d1)), is.null(colnames(d2))))
    if ((colStatus == 1) | ((colStatus == 0) & (any(colnames(d1) != colnames(d2)))))
      msg <- c(msg, paste(d1.d2, "have different column names."))
    
    # Check rownames
    rowStatus <- sum(c(is.null(rownames(d1)), is.null(rownames(d2))))
    if ((rowStatus == 1) | ((rowStatus == 0) & (any(rownames(d1) != rownames(d2)))))
      msg <- c(msg, paste(d1.d2, "have different row names."))

    # if they are data frames, change factors to characters
    if (is.data.frame(d1))
      d1 <- factor2character(d1)
    if (is.data.frame(d2))
      d2 <- factor2character(d2)

    # Change character NA's to special characters "9999999"
    changeNAchar <- function(x) 
      ifelse(is.na(x), "999999999999", as.character(x))
     
    # Function for checking equivalence of numerical values
    cNum <- function(vec1, vec2) {

      # Check NA's
      v1NA <- is.na(vec1)
      v2NA <- is.na(vec2)

      # If only 1 is NA, set them to different numbers
      if (any(oneNA <- v1NA | v2NA)) {
        vec1[oneNA] <- 9999999
        vec2[oneNA] <- 9

        # If they're both NA, set them equal to the same number
        if (any(bothNA <- v1NA & v2NA)) {
          vec1[bothNA] <- 99999
          vec2[bothNA] <- 99999
        }
      }
  
      # Check +Inf
      v1Inf <- vec1 == Inf
      v2Inf <- vec2 == Inf

      # If only 1 is Inf, set them to different numbers
      if (any(oneInf <- v1Inf | v2Inf)) {
        vec1[oneInf] <- 9999999
        vec2[oneInf] <- 9
        
        # If they're both Inf, set them to the same number
        if (any(bothInf <- v1Inf & v2Inf)) {
          vec1[bothInf] <- 99999
          vec2[bothInf] <- 99999
        }
      }

      # Check -Inf
      v1nInf <- vec1 == -Inf
      v2nInf <- vec2 == -Inf

      # If only 1 is -Inf, set them to different numbers
      if (any(onenInf <- v1nInf | v2nInf)) {
        vec1[onenInf] <- 9999999
        vec2[onenInf] <- 9
        
        # If they're both -Inf, set them to the same number
        if (any(bothnInf <- v1nInf & v2nInf)) {
          vec1[bothnInf] <- 99999
          vec2[bothnInf] <- 99999
        }        
      }


      # Perfect equivalence
      e1 <- vec1 == vec2
      
      # Absolute error equivalence
      e2 <- abs(vec1 - vec2) < maxAbsError

      # Relative error equivalence
      e3 <- ifelse(abs(vec2) > abs(vec1),
                   abs((vec1 - vec2) / vec2) < maxRelError,
                   abs((vec1 - vec2) / vec1) < maxRelError)

      # convert any NAs or NaN's to FALSE
      e3[is.na(e3)] <- FALSE

      # Checking for NA's in e1 and e2
      if (any(is.na(e1)))
        warning("in cNum() in dframeEquiv():  'e1' contains an unexpected NA\n", call. = FALSE)
      if (any(is.na(e2)))
        warning("in cNum() in dframeEquiv():  'e2' contains an unexpected NA\n", call. = FALSE)          

      # If we have one of the 3 equivalences, return TRUE
      return(e1 | e2 | e3)

    } # cNum

    # Check each column for equivalence
    for (i in 1:NCOL(d1)) {

      v1 <- d1[,i]
      v2 <- d2[,i]

      both.num <- sum(c(is.numeric(v1), is.numeric(v2)))

      if (both.num == 2)
        equiv.mat <- cbind(equiv.mat, cNum(v1,v2))

      else {
        v1 <- changeNAchar(v1)
        v2 <- changeNAchar(v2)
        equiv.mat <- cbind(equiv.mat, v1 == v2)
## This doesn't catch all the different ways in which two columns could have different data types        
#        if (both.num == 1)   
#          msg <- c(msg, paste("Column", i, "of", d1.d2, "do not have same data type.",
#                   "Will coerce them to characters in order to compare them.")
      }
    } # for (i

    # Fraction of elements that are equivalent
    frac.equiv <- sum(equiv.mat)/(NROW(d1)*NCOL(d1))
     
    if (!all(equiv.mat)) {
        msg <- c(msg, paste(round(100*frac.equiv,2),
                            "% of the elements of ", d1.d2, " are equivalent.", sep=""))

      # Get the row,column locations of inequivalencies
      locations <- which(!equiv.mat)
      loc.inequiv <- data.frame(row.loc = ifelse(locations %% NROW(d1) == 0,
                                                 NROW(d1), locations %% NROW(d1)),
                                col.loc = ifelse(locations %% NROW(d1) == 0,
                                                 locations %/% NROW(d1),
                                                 (locations %/% NROW(d1)) + 1))
      keep.checking <- FALSE
    }
    else 
      msg <- c(msg, paste("All elements of", d1.d2, "appear to be equivalent."))
    
  }

  if (verbose)
    cat(paste(msg, "\n", sep=""))

  invisible(list(equiv=keep.checking, msg=msg, frac.equiv=frac.equiv,
                 loc.inequiv=loc.inequiv, equiv.matrix=equiv.mat))

}# end dframeEquiv

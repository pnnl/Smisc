##' Produces all possible combinations of a set of linear model predictors
##'
##' Produces a list representing all possible combinations of linear model
##' predictors
##'
##' Uses \code{\link{combn}} to identify the combinations.
##'
##' @export
##' @param n.pred integer indicating the number of predictors
##'
##' @param outFile text string indicating the .Rdata file to which the returned
##' list of predictor combinations will be saved.  If NULL, then no file is
##' saved.
##'
##' @param njobs Integer indicating the number of parallel jobs to be used in
##' calculating the combinations, using \code{\link{parLapplyW}}
##'
##' @return A list of class \code{combolist} is invisibly returned with the two
##' components shown below.  If \code{outFile} is not \code{NULL}, this same
##' list is saved to \code{outFile}: \item{len}{The total number of
##' combinations} \item{pList}{A list where each element contains an integer
##' representation of one combination of the predictors}
##'
##' @author Landon Sego
##'
##' @keywords misc
##'
##' @examples
##' x <- comboList(4)
##' print(x)
##'
##' # A parallel job
##' y <- comboList(4, njobs = 2)
##'
##' # Should be equal
##' identical(x, y)
comboList <- function(n.pred, outFile = NULL, njobs = 1) {

  # Basic checks on the arguments
  stopifnot(is.numeric(n.pred),
            n.pred >= 1,
            if (!is.null(outFile)) is.character(outFile) else TRUE,
            is.numeric(njobs),
            njobs >= 1)

  # Create the profile of counts for each category
  counts <- choose(n.pred, 1:n.pred)
  groups <- c(1:n.pred)[order(counts, decreasing = TRUE)]

  # Make sure we don't spawn more jobs then there are groups
  if (njobs > n.pred) {
    njobs <- n.pred
  }

  # Create a list that will spread the combn work out somewhat evenly
  cnt.list <- vector(njobs, mode="list")
  for (i in 1:njobs) {
    if (i < njobs)
      cnt.list[[i]] <- groups[i]
    else
      cnt.list[[njobs]] <- groups[njobs:length(groups)]
  }

  # Wrapper function for combn
  combn.wrapper <- function(n.to.choose) {

    if (length(n.to.choose) == 1)
      return(utils::combn(n.pred, n.to.choose, simplify = FALSE))

    # If the list element has length greater than 1 (these should be the fast cases)
    else {

      for (n in n.to.choose)
        assign(paste("o", n, sep=""), utils::combn(n.pred, n, simplify = FALSE))

      # concatentate all these lists together at once
      out.text <- paste("c(", paste(paste("o", n.to.choose, sep=""), collapse=","), ")", sep="")

      return(eval(parse(text = out.text)))

    }

  } # combn.wrapper


  # Run jobs in serial
  if (njobs == 1) {

    out <- lapply(cnt.list, combn.wrapper)

  }

  # Or run in parallel
  else {

    out <- parLapplyW(cnt.list, combn.wrapper, njobs = njobs, varlist = "n.pred")

  }

  # Unpack the list
  out <- unlist(out, recursive = FALSE)

  # Final check
  if (length(out) != sum(counts))
    stop("comboList produced ", length(out), " models, when it should have produced ", sum(counts), ".")

  cList <- list(len = sum(counts), pList = out)
  class(cList) <- "combolist"

  # Write an output file if requested
  if (!is.null(outFile)) {
    save(cList, file = outFile)
  }

  # Invisibly return the list
  invisible(cList)

} # comboList

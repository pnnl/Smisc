##' Wrapper for installing packages from various repositories
##'
##' Wrapper for installing packages from various repositories
##'
##' The \code{r = "CRAN"} option points to the Fred Hutchinson Cancer Research Center in Seattle,
##' \url{http://cran.fhcrc.org}.
##' The PNNL repository is at \url{http://martingale.pnl.gov/computing/repos}.
##' The Bioconductor repository is at \url{http://bioconductor.org}
##' And Ryan Hafen's github repository is at \url{https://github.com/hafen}
##'
##' @export
##'
##' @param packages Character vector of package names that are in a single repository
##'
##' @param r A single character string indicating the repository (may be abbreviated).  Defaults to \code{CRAN}. See Details.
##'
##' @param d Logical indicating whether \code{dependencies = TRUE} in \code{\link{install.packages}}.
##'
##' @seealso \code{\link{install.packages}}
##'
##' @return Nothing

ip <- function(packages, r = c("CRAN","PNNL","Bioconductor", "Hafen"), d = TRUE) {
  
 r <- match.arg(r)

 if (r == "Bioconductor") {

    source("http://bioconductor.org/biocLite.R")
    biocLite(packages)

 }

 else if (r == "Hafen") {

   if (!("devtools" %in% installed.packages()))
     install.packages("devtools", repos = "http://cran.fhcrc.org", dependencies = TRUE)
   
   stopifnot(require(devtools))

   for (p in packages)
     install_github(p, "hafen")

 }

 else {
 
   repos <- switch(r,
                   "CRAN" = "http://cran.fhcrc.org",
                   "PNNL" = "http://martingale.pnl.gov/computing/repos")
   
   install.packages(packages, repos = repos, dependencies = d)

 }

 invisible(NULL)
 
} # ip

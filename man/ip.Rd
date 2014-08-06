\name{ip}
\alias{ip}
\title{Wrapper for installing packages from various repositories}
\usage{
  ip(packages,
    r = c("CRAN", "PNNL", "Bioconductor", "Hafen"),
    d = TRUE)
}
\arguments{
  \item{packages}{Character vector of package names that
  are in a single repository}

  \item{r}{A single character string indicating the
  repository (may be abbreviated).  Defaults to
  \code{CRAN}. See Details.}

  \item{d}{Logical indicating whether \code{dependencies =
  TRUE} in \code{\link{install.packages}}.}
}
\value{
  Nothing
}
\description{
  Wrapper for installing packages from various repositories
}
\details{
  The \code{r = "CRAN"} option points to the Fred
  Hutchinson Cancer Research Center in Seattle,
  \url{http://cran.fhcrc.org}. The PNNL repository is at
  \url{http://martingale.pnl.gov/computing/repos}. The
  Bioconductor repository is at
  \url{http://bioconductor.org} And Ryan Hafen's github
  repository is at \url{https://github.com/hafen}
}
\seealso{
  \code{\link{install.packages}}
}


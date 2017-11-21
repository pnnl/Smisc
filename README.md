## Smisc

An R package containing a collection of functions for statistical computing, data manipulation, and visualization.  You can find package vignettes and documentation [here](https://pnnl.github.io/Smisc).

#### To cite:

Sego LH. 2016. Smisc: Sego Miscellaneous. A collection of functions for statistical computing and data manipulation in R.
Pacific Northwest National Laboratory. https://pnnl.github.io/Smisc.

#### To install:

You can install the package from [CRAN](https://CRAN.R-project.org/) or from [GitHub](https://github.com):

##### CRAN

    install.packages("Smisc")

##### GitHub

You can install from  [GitHub](https://github.com) using the [devtools](https://CRAN.R-project.org/package=devtools) package,
in which case you'll need these prerequistites in order to compile the C code in the `Smisc` package:

- Mac: you'll need [Xcode](https://developer.apple.com/xcode/)
- Windows: you'll need to install [R tools](https://CRAN.R-project.org/bin/windows/Rtools/)
- Linux/Unix: compilation should take place automatically

Now do the following in R:

    # If devtools is not already installed, do this:
    install.packages("devtools") 

    # Now install the package
    devtools::install_github("pnnl/Smisc")

#### Acknowledgements:

This package was developed with support from the Signature Discovery Initiative at Pacific Northwest National Laboratory, conducted under the Laboratory Directed Research and Development Program at PNNL, a multiprogram national laboratory operated by Battelle for the U.S. Department of Energy. 


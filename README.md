### Smisc (Sego Miscellaneous) 

An R package containing a collection of functions for statistical computing and data manipulation.  Please cite it as follows:

Sego LH. 2015. Smisc: Sego Miscellaneous. A collection of functions for statistical computing and data manipulation in R.
Pacific Northwest National Laboratory. http://github.com/pnnl/Smisc.

To install:

    library(devtools)
    install_github("pnnl/Smisc")

A few notes about installation:
- The `devtools` package is available from [CRAN](http://cran.r-project.org)
- The `Smisc` package contains C code that requires compilation:  
On a Mac, you'll need [Xcode](http://developer.apple.com/xcode/), 
on Windows, you'll need to install [R tools](http://cran.r-project.org/bin/windows/Rtools/), 
and on Linux, compilation should take place "automatically."

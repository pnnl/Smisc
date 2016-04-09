### Smisc

An R package containing a collection of functions for statistical computing and data manipulation.  You can find package documentation [here](https://pnnl.github.io/Smisc).

#### To cite:

Sego LH. 2016. Smisc: Sego Miscellaneous. A collection of functions for statistical computing and data manipulation in R.
Pacific Northwest National Laboratory. http://pnnl.github.io/Smisc.

#### To install:

Before you get started, the source code of the `Smisc` package contains C code that requires compilation:
  
- Mac: you'll need [Xcode](https://developer.apple.com/xcode/)
- Windows: you'll need to install [R tools](http://cran.r-project.org/bin/windows/Rtools/)
- Linux/Unix: compilation should take place automatically

Then type the following in R:

    # Install the 'devtools' package if needed
    install.packages("devtools") 

    devtools::install_github("pnnl/Smisc")

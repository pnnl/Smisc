Version 0.2.7, 2015-01-31
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added option to bind using `c()` in `qbind()`
- Modified argument to `loadObject()` so that multiple files can be loaded at
  once. If only a single file is provided, it behaves just as it did before, so
  it is backward compatible.

BUG FIXES

- `list2df()` now correctly handles a list of lists
- fixed `qbind()` to work properly when called from inside another function

Version 0.2.6, 2015-01-24
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- New function: `hardCode()`, for printing code to create a particular vector
- New function: `qbind()`, for quickly row or column binding numerous objects
- Addition of the "fft" method to `dkbinom()` and `pkbinom()` for validation
- Deprecated:  `d2binom()` and `p2binom()`, as they duplicate `dkbinom()` and `pkbinom()`

Version 0.2.5, 2014-11-25
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Stream editing gets a new function, `sed_comment()`, to facilitate commenting and
  uncommenting single lines of code.

Version 0.2.4, 2014-11-12
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- `pvar()` gets a new argument, 'sep'.
- Removed `ifelse1()`, as it duplicates native code (e.g., if (cond) val1 else val2)
  that is almost as easy to type.

Version 0.2.3, 2014-10-30
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added `hpd()`, a utility for finding highest posterior density intervals for
  unimodal density functions.
- Added the 'nJobs' argument to `plotFun()` for parallel processing.  
- Added `doCallParallel()`, a wrapper for parallelizing function calls over vector inputs.

Version 0.2.2, 2014-10-20
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added `select()`, a function for subsetting matrices or dataframes by rows or
  columns and returning a matrix or dataframe if only a single row or column was
  selected.

Version 0.2.1, 2014-10-08
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added `ifelse1()`, a non-vectorized version of `ifelse()`. 
- Added `plotFun()`, for plotting one or more functions over a common domain.

Version 0.2.0, 2014-09-30
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added `sed_insert()`, `sed_replace()`, `sed_substitute()`, `streamEdit()` for easy
  sed-like editing of text files (or character vectors).
- Added `more()`, a function to quickly show the contents of a text file on the
  console.

Version 0.1.1, 2014-08-08
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Modified `loadObject()` so that the .Rdata file can contain more than one object,
  in which case `loadObject()` returns a list.

Version 0.1.0, 2014-08-06
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- These functions were lifted from the pnlStat package in preparation for pushing
  this package to CRAN.


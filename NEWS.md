Version 0.3.9, 2017-11-20
-----------------------------------------------------------------------------------

FIXES

- Email address update in `Smisc-package.Rd`, corrections to URLs


Version 0.3.8, 2017-11-14
-----------------------------------------------------------------------------------

FIXES

- System calls to R in `plapply()` corrected to call the version of R being used, as opposed to the one in the search path (thanks to Kurt Hornik for the feedback)

FEATURES / CHANGES

- Updates and minor edits to documentation


Version 0.3.7, 2016-07-04
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Numerical tests for `dkbinom()` relaxed to pass on SparcSolaris in the CRAN check


Version 0.3.6, 2016-06-22
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- `\donttest{}` added to examples for `timeIt()`, `dkbinom()`, `pddply()`, `hpd()`, and `dfplapply()` to appease CRAN.
- Additional language added to `Description` in `DESCRIPTION`.


Version 0.3.5, 2016-06-20
-----------------------------------------------------------------------------------

FIXES

- `select()` modified to correctly handle selecting from (n x 1) or (1 x n) dataframes and matrices.  Additional tests added.

FEATURES / CHANGES

- Licence modified to BSD_3_clause + file LICENSE


Version 0.3.4, 2016-06-15
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Removed `sortDF()`, `dbb()`, `qbb()`, `pbb()`, `rbb()`, and `interactionPlot()` due to licensing prior to submission to CRAN


Version 0.3.3, 2016-05-13
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Addition of `cusum()`, which calculates all the statistics needed for the upper one-sided CUSUM chart
- Revision of `movAvg2()`:  Addition of `print()` and `plot()` methods for the object returned by `movAvg2()`.  The `plot` argument is dropped, and argument checks with descriptive error messages are added. Documentation revisions.


Version 0.3.2, 2016-05-04
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Tests created for `padZero()`
- Error message refinement for `selectElements()`, `select()`


Version 0.3.1, 2016-04-13
-----------------------------------------------------------------------------------

FIXES

- `pkbinom()` and `dkbinom()` now gracefully handle NAs as well as values outside the support of the the sum of the binomial variates
- `hardCode()` now correctly handles NA's for character vector inputs

FEATURES / CHANGES

- `checkArgs` argument removed from `dkbinom()` and `pkbinom()`
- `stopifnotMsg()` gets a `level` argument 
- Tests created for `integ()`, `cumMax()`, `as.numericSilent()`, `cumsumNA()`, `umvueLN()`, `rma()`, `timeStamp()`, `dkbinom()`, `pkbinom()`, `pvar()`, `hardCode()`, `more()`, and `stripExtension()`
- Minor documentation edits


Version 0.3.0, 2016-04-09
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Stream editing functions `streamEdit()`, `sed_insert()`, `sed_replace()`, `sed_substitute()`, and `sed_comment()` become their own package: [rsed](https://pnnl.github.io/rsed)
- `umvue.ln()` renamed `umvueLN()`, and it gets `tol` and `verbose` arguments
- `factor2num()` renamed `factor2numeric()`
- `validEmailAddress()` is deprecated
- `more()` gets a `display` argument to view the head, tail, or all of the file
- More descriptive error messages for `selectElements()`, `doCallParallel()`, `plapply()`, `dfplapply()`, `plotFun()`, `smartTimeAxis()`, `vertErrorBar()`, `interactionPlot()`, `hpd()`, and `umvueLN()`
- Several arguments to `plapply()` and `dfplapply()` are modified:  `jobName` becomes `workDir`, `clobber` is added, `rout` can now specifiy a filename where all the `.Rout` files will be collected
- The `nJobs` argument of `doCallParallel()`, `hpd()`, and `plotFun()` is renamed `njobs` for consistency with other 
parallelization functions
- The `height` argument of `vertErrorBar()` is changed to `barLength`
- The `verbose` argument of `comboList()` is removed, as it served no purpose
- If a `csv` file is passed to `dataIn()`, character strings are now converted to factors, consitent with R's default behavior
- The `jitterErrorBars` argument of `interactionPlot()` is now a list that passes arguments to `jitter()`, giving more control of the jittering
- Tests created for `select()`, `doCallParallel()`, `plapply()`, `dfplapply()`, `parseJob()`, `openDevice()`, `plotFun()`, `vertErrorBar()`, and `interactionPlot()`
- Numerous documentation improvements


Version 0.2.17, 2016-02-01
-----------------------------------------------------------------------------------

FIXES

- `stopifnotMsg()` can be now be called from the global environment as well as within functions

Version 0.2.16, 2016-01-25
-----------------------------------------------------------------------------------

FIXES

- Correction to documentation of `strings.as.factors` argument of `list2df()`
- Correction to documentation of `.variables` argument of `pddply()`
- `selectElements()` no longer allows the `elements` argument to be a vector of 0's and 1's because it led to logical inconsistencies

FEATURES / CHANGES

- Added tests for `allMissing()` and beta-binomial functions
- Added `stopifnotMsg()` to make it easier to return error messages when checking the arguments of functions

Version 0.2.15, 2015-10-31
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Addition of `testthat` test structure
- Addition of tests for `pddply()`

Version 0.2.14, 2015-10-30
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Addition of `parLapplyW()` and `pddply()`
- Various edits to prepare `Smisc` for submission to CRAN
- Support for Cairo graphics for `openDevice()` is removed


Version 0.2.13, 2015-10-12
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- `getExtension()` gains `split.char` argument


Version 0.2.12, 2015-09-15
-----------------------------------------------------------------------------------

FIXES

- `plotFun()` was making calls to a deprecated function, `ifelse1()`.  This was
   changed to `if (cond) x else y`.

Version 0.2.11, 2015-09-02
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Created more descriptive error messages for `selectElements()`

Version 0.2.10, 2015-08-08
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added `sepList()` to easily separate elements in a list to separate objects

Version 0.2.9, 2015-06-17
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added `selectElements()` for robustly selecting rows or columns from a data frame
- Added `factor2num()` for converting factors to numeric values


Version 0.2.8, 2015-02-12
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added flexbility to `select()` by permitting the selection of 0 rows or columns

FIXES

- Corrected one of the error messages in `select()`

Version 0.2.7, 2015-01-31
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added option to bind using `c()` in `qbind()`
- Modified argument to `loadObject()` so that multiple files can be loaded at
  once. If only a single file is provided, it behaves just as it did before, so
  it is backward compatible.

FIXES

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

- `pvar()` gets a new argument, `sep`.
- Removed `ifelse1()`, as it duplicates native code (e.g., `if (cond) val1 else val2`)
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


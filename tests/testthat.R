library(testthat)
library(Smisc)
suppressWarnings(RNGversion("3.5.0"))

# There shouldn't be any files in the tmp folder
unlink("testthat/tmp", recursive = TRUE, force = TRUE)
dir.create("testthat/tmp")

test_check("Smisc")

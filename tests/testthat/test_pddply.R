context("pddply()")

test_that("pddply() returns values that are identical to ddply()", {

  library(plyr)
  data(baseball)
  
  # First round of checks
  o1 <- pddply(baseball, ~ year, nrow, njobs = 2)
  o2 <- ddply(baseball, ~ year, nrow)
  o3 <- pddply(baseball, ~ year, nrow, njobs = 1)
  expect_equal(o1, o2)
  expect_equal(o2, o3)
  expect_equal(o1, o3)
  
  # Different example
  o4 <- pddply(baseball, "lg", c("nrow", "ncol"), njobs = 2)
  o5 <- ddply(baseball, "lg", c("nrow", "ncol"))
  expect_equal(o4, o5)
   
})

test_that("ppdply() catches errors", {

  d <- data.frame(g = rep(letters[1:3], each = 5), x = rnorm(15))

  # Create a function that should throw an error
  f <- function(x) {
    mean(x$x) + nonExistentObject
  }

  expect_error(pddply(d, "g", f, njobs = 2), "object 'nonExistentObject' not found")
  
})

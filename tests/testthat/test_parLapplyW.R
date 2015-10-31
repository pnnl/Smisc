context("parLapplyW()")

test_that("parLapplyW() returns values that are identical to lapply()", {

  a <- list(a = rnorm(10), b = rnorm(20), c = rnorm(15))
  
  res.1 <- parLapplyW(a, mean, njobs = 2)
  res.2 <- parLapplyW(a, mean, njobs = 1)
  res.3 <- lapply(a, mean)

  expect_equal(res.1, res.3)
  expect_equal(res.2, res.3)
    
})

test_that("parLapplyW() catches errors", {

  d <- list(a1 = rnorm(10), a2 = rnorm(10), a3 = rnorm(10), a4 = rnorm(10))

  # Create a function that should throw an error
  f <- function(x) {
    mean(x) + nonExistentObject
  }

  expect_error(parLapplyW(d, f, njobs = 2), "object 'nonExistentObject' not found")
  
})

context("pddply()")

test_that("pddply() returns values that are identical to ddply()", {

  loadNamespace("plyr")
  data(baseball, package = "plyr")
  
  # First round of checks
  o1 <- pddply(baseball, ~ year, nrow, njobs = 2)
  o2 <- plyr::ddply(baseball, ~ year, nrow)
  o3 <- pddply(baseball, ~ year, nrow, njobs = 1)
  expect_equal(o1, o2)
  expect_equal(o2, o3)
  expect_equal(o1, o3)
  
  # Different example
  o4 <- pddply(baseball, "lg", c("nrow", "ncol"), njobs = 2)
  o5 <- plyr::ddply(baseball, "lg", c("nrow", "ncol"))
  expect_equal(o4, o5)
   
})

test_that("We can pass objects and packages into ddply()", {

  # A nonsense example where we need to pass objects and packages into the cluster
  number1 <- 7
  
  f <- function(x, number2 = 10) {
    paste(x$id[1], padZero(number1, num = 2), number2, sep = "-")
  }
  
  # In parallel
  o5 <- pddply(baseball[1:100,], "year", f, number2 = 13, njobs = 2,
               .paropts = list(.packages = "Smisc", .export = "number1"))
  
  # Non parallel
  o6 <- plyr::ddply(baseball[1:100,], "year", f, number2 = 13)
  
  expect_equal(o5, o6)
    
})

test_that("ppdply() catches errors", {

  d <- data.frame(g = rep(letters[1:3], each = 5), x = rnorm(15))

  # Create a function that should throw an error
  f <- function(x) {
    mean(x$x) + nonExistentObject
  }

  expect_error(pddply(d, "g", f, njobs = 2), "object 'nonExistentObject' not found")
  
})

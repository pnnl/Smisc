context("hardCode()")

test_that("hardCode() returns messages as expected", {

  expect_output(hardCode(1:5, vname = "a", vert = FALSE), "a <- c(1, 2, 3, 4, 5)", fixed = TRUE)

  expect_output(hardCode(1:2, vname = "new"), "new <- c(1,\n         2)", fixed = TRUE)

  expect_output(hardCode(letters[1:3], vname = "b", vert = FALSE), 'b <- c("a", "b", "c")', fixed = TRUE)

  expect_output(hardCode(c(TRUE, FALSE)), "x <- c(TRUE,\n       FALSE)", fixed = TRUE)

  expect_output(hardCode(c(4 + 3i, 2 - 2i), vert = FALSE), "x <- c(4+3i, 2-2i)", fixed = TRUE)
  
})

test_that("hardCode() returns errors as expected", {

  expect_error(hardCode(list(x = rnorm(3))), "'x' must be a numeric, character")
  
  expect_error(hardCode(1:2, vname = 7), "'vname' must be a character string")

  expect_error(hardCode("a", vert = "this"), "'vert' must be TRUE or FALSE")
                        
})

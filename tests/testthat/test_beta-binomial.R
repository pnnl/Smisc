context("Beta-binomial functions")

test_that("qbb() and pbb() are inverses", {

  expect_equal(qbb(pbb(0:24, 29, 1, 33), 29, 1, 33), 0:24)
  expect_equal(qbb(pbb(0:15, 15, 10, 5), 15, 10, 5), 0:15)
    
})

test_that("pbb() behaves as a cdf", {

  expect_equal(pbb(c(-1, -2, 15, 16), 15, 1, 1), c(0, 0, 1, 1))
  expect_equal(pbb(c(-1, 7, 20), 7, 0.3, 10), c(0, 1, 1))
  expect_equal(pbb(c(-0.1, 21), 20, 15, 2), c(0, 1))
    
})

test_that("qbb() behaves as an inverse cdf", {

  expect_equal(qbb(c(0, 1), 15, 1, 1), c(0, 15))
  expect_equal(qbb(c(0, 1), 7, 0.3, 10), c(0, 7))
  expect_equal(qbb(c(0, 1), 20, 15, 2), c(0, 20))

  # These should be in between the max and the min
  x1 <- qbb(0.5, 15, 1, 1)
  x2 <- qbb(0.5, 5, 0.5, 0.5)
  x3 <- qbb(0.5, 20, 2, 40)
  expect_true(0 <= x1 & x1 <= 15)
  expect_true(0 <= x2 & x2 <= 5)
  expect_true(0 <= x3 & x3 <= 20)
    
})

test_that("extremes of dbb() are correct", {

  expect_equal(dbb(c(-1, 31), 12, 3, 0.2), c(0, 0))
  expect_equal(dbb(c(-1, 40), 30, 0.5, 17), c(0, 0))
  expect_equal(dbb(c(0, -1, 5, 20, 31), 30, 5, 5)[c(2,5)], c(0,0))
  expect_equal(dbb(377, 5, 2, 3), 0)
  expect_equal(dbb(0, 0, 0.1, 1), 1)
  expect_equal(dbb(0, 0, 5, 320), 1)
    
})

test_that("dbb() agrees with pbb()", {

  expect_equal(sum(dbb(0:10, 20, 0.5, 18)), pbb(10, 20, 0.5, 18))
  expect_equal(sum(dbb(0:30, 28, 2, 2)), pbb(30, 28, 2, 2))
  expect_equal(sum(dbb(0:19, 28, 2, 2)), pbb(19, 28, 2, 2))  
  expect_equal(dbb(0, 7, 2, 0.1), pbb(0, 7, 2, 0.1))
  expect_equal(sum(dbb(0:5, 5, 37, 2)), 1)
     
})

test_that("rbb() ends up in the correct range of values", {

  x <- rbb(20, 15, 8, 0.7)
  expect_true(all(0 <= x & x <= 15))
  
  x <- rbb(100, 200, 20, 70)
  expect_true(all(0 <= x & x <= 200))
  
  x <- rbb(50, 19, 0.5, 0.5)
  expect_true(all(0 <= x & x <= 19))
    
})

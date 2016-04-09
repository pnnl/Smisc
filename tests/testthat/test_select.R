context("select()")

# Consider this data frame
d <- data.frame(a = 1:5, b = rnorm(5), c = letters[1:5], d = factor(6:10),
                row.names = LETTERS[1:5], stringsAsFactors = FALSE)

test_that("We get identical behavior when selecting more than one column", {

  d1 <- d[, c("d", "c")]
  d1c <- select(d, c("d", "c"))
  expect_equal(d1, d1c)

})

test_that("Selecting a single row from a data frame produces results identical to default R behavior", {

  d2 <- d[2,]
  d2c <- select(d, "B", cols = FALSE)
  expect_equal(d2, d2c)

})

# Now consider a matrix
m <- matrix(rnorm(20), nrow = 4, dimnames = list(LETTERS[1:4], letters[1:5]))

test_that("Column selection with two or more or more columns is equivalent to default R behavior", {
  
  m1 <- m[,c(4, 3)]
  m1c <- select(m, c("d", "c"))
  expect_equal(m1, m1c)

})

test_that("Selecting a single row returns a matrix of 1 row instead of a vector", {
    
  m2 <- m["C",]
  m2c <- select(m, "C", cols = FALSE)
  expect_true(!is.matrix(m2))
  expect_true(is.matrix(m2c))

})

test_that("Selecting a dataframe using indexes and names achieves the same result", {

  # Name selections
  a1 <- select(d, "a")
  a2 <- select(d, c("a","d"))
  a3 <- select(d, c("c","b","d"))
  a4 <- select(d, c("a","d"))
  a5 <- select(d, c("c"))
  a6 <- select(d, c("d"))

  # Index selections
  a1c <- select(d, 1)
  a2c <- select(d, c(1, 4))
  a3c <- select(d, c(3, 2, 4))
  a4c <- select(d, c(1, 4))
  a5c <- select(d, 3)
  a6c <- select(d, 4)

  # Check results
  expect_equal(a1, a1c)
  expect_equal(a2, a2c)
  expect_equal(a3, a3c)
  expect_equal(a4, a4c)
  expect_equal(a5, a5c)
  expect_equal(a6, a6c)

  # Now try rows
  b1 <- select(d, c(4, 1), cols = FALSE)
  b2 <- select(d, 2, cols = FALSE)
  b3 <- select(d, 4, cols = FALSE)

  b1c <- select(d, c("D", "A"), cols = FALSE)
  b2c <- select(d, "B", cols = FALSE)
  b3c <- select(d, "D", cols = FALSE)

  # Check results
  expect_equal(a1, a1c)
  expect_equal(a2, a2c)
  expect_equal(a3, a3c)

 })

test_that("Selecting a matrix using indexes and names achieves the same result", {

  # Checks for the matrix stuff
  m3 <- select(m, c("d", "c"))
  m3c <- select(m, c(4, 3))
  m4 <- select(m, "b")
  m4c <- select(m, 2)
  m5 <- select(m, c("D", "C"), cols = FALSE)
  m5c <- select(m, c(4, 3), cols = FALSE)
  m6 <- select(m, "B", cols = FALSE)
  m6c <- select(m, 2, cols = FALSE)

  # Check results
  expect_equal(m3, m3c)
  expect_equal(m4, m4c)
  expect_equal(m5, m5c)
  expect_equal(m6, m6c)

})

test_that("Check 0 rows and 0 column results", {
    
  dn <- data.frame(a = 1:10, b = 11:20, c = 21:30)
  rownames(dn) <- letters[1:10]
  mn <- as.matrix(dn)
  
  # Check results
  expect_equal(select(dn, 0), dn[,0])
  expect_equal(select(dn, character(0)), dn[,0])
  expect_equal(select(mn, 0), mn[,0])
  expect_equal(select(mn, character(0)), mn[,0])
  expect_equal(select(dn, 0, cols = FALSE), dn[0,])
  expect_equal(select(dn, character(0), cols = FALSE), dn[0,])
  expect_equal(select(mn, 0, cols = FALSE), mn[0,])
  expect_equal(select(mn, character(0), cols = FALSE), mn[0,])

})

context("interactionPlot()")

# Some data
d <- data.frame(Factor_1 = c(rep("a", 10), rep("b", 10)),
                Factor_2 = rep(c("c", "d"), each = 5),
                response = c(rnorm(5, mean = 5),
                             rnorm(5, mean = 8),
                             rnorm(5, mean = 7),
                             rnorm(5, mean = 5.5)))
  
# Get the MSE
MSE <- summary(lm(response ~ Factor_1 * Factor_2, data = d))$sigma^2
  
# Calculate the total height of the bars, based on Fisher's LSD
LSD <- qt(0.05 / 2, 16, lower.tail = FALSE) * sqrt(2 * MSE / 5)

test_that("Errors work as expected for 'errorBar'", {

  err1 <- "'errorBar' must be a list of atomic, numeric elements with names 'barLength', 'width'"
    
  # errorBar not a list
  expect_error(with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                                       errorBar = "something")),
               err1)

  # errorBar missing non-atomic elements
  expect_error(with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                                       errorBar = list(barLength = LSD, width = c(0.2, 0.3)),
                                       jitterErrorBars = list(factor = 1))),
               err1)

  # errorBar missing width
  expect_error(with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                                       errorBar = list(barLength = LSD),
                                       jitterErrorBars = list(factor = 1))),
               err1)

  # errorBar with extra args
  expect_error(with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                                       errorBar = list(barLength = LSD, width = 0.05, fake = 7))),
               err1)

  # errorBar gwith non-numeric args
  expect_error(with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                                       errorBar = list(barLength = LSD, width = "a"),
                                       jitterErrorBars = list(factor = 1))),
               err1)


})

test_that("Errors work as expected for 'jitterErrorBars'", {
  
  err2 <- "'jitterErrorBars' must be a list of atomic, numeric arguments with names 'amount' and"
  
  # jitterErrorBars not a list
  expect_error(with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                                       errorBar = list(barLength = LSD, width = 0.05),
                                       jitterErrorBars = TRUE)),
               err2)

  # jitterErrorBars has wrong args
  expect_error(with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                                       errorBar = list(barLength = LSD, width = 0.05),
                                       jitterErrorBars = list(amount = 7, fat = 2))),
               err2)

  # jitterErrorBars has too many args
  expect_error(with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                                       errorBar = list(barLength = LSD, width = 0.05),
                                       jitterErrorBars = list(amount = 7, factor = 2, now = "this"))),
               err2)

  # jitterErrorBars has n arg that's not numeric
  expect_error(with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                                       errorBar = list(barLength = LSD, width = 0.05),
                                       jitterErrorBars = list(amount = 7, factor = "2"))),
               err2)

  # jitterErrorBars has an arg that's not atomic
  expect_error(with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                                       errorBar = list(barLength = LSD, width = 0.05),
                                       jitterErrorBars = list(amount = 7, factor = c(3,1)))),
               err2)

  
})

test_that("interation plot is produced", {

  # First call, with jittering    
  f <- "test_interaction_plot_1.pdf"

  pdf(file = f)

  with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                          errorBar = list(barLength = LSD, width = 0.05),
                          jitterErrorBars = list(factor = 1)))

  dev.off()

  expect_true(file.exists(f))

  unlink(f)


  # Second call without jittering
  f <- "test_interaction_plot_2.pdf"
  
  pdf(file = f)

  with(d, interactionPlot(Factor_1, Factor_2, response, las = 1,
                          errorBar = list(barLength = LSD, width = 0.05)))

  dev.off()

  expect_true(file.exists(f))

  unlink(f)

  # Third call with no error bars
  f <- "test_interaction_plot_3.pdf"
  
  pdf(file = f)

  with(d, interactionPlot(Factor_1, Factor_2, response))

  dev.off()

  expect_true(file.exists(f))

  unlink(f)

  # A call with ignored jitterErrorBar args
  f <- "test_interaction_plot_4.pdf"

  pdf(file = f)

  with(d, interactionPlot(Factor_1, Factor_2, response,
                          jitterErrorBars = list(fake = 1)))

  dev.off()

  expect_true(file.exists(f))

  unlink(f)

  
})

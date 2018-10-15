context("Print")



test_that("Normal Printing", {

  dp <- DirichletProcessGaussian(rnorm(10))

  expect_error(capture.output(print(dp)), NA)
  expect_error(capture.output(print(dp, param_summary = TRUE)), NA)

  dpfit <- Fit(dp, 2, progressBar = FALSE)
  expect_error(capture.output(print(dpfit)), NA)
  expect_error(capture.output(print(dpfit, param_summary = TRUE)), NA)

})

test_that("Exp Printing", {

  dp <- DirichletProcessExponential(rexp(10))

  expect_error(capture.output(print(dp)), NA)
  expect_error(capture.output(print(dp, param_summary = TRUE)), NA)

  dpfit <- Fit(dp, 2, progressBar = FALSE)
  expect_error(capture.output(print(dpfit)), NA)
  expect_error(capture.output(print(dpfit, param_summary = TRUE)), NA)
})

test_that("Beta Printing", {

  capture.output(dp <- DirichletProcessBeta(rbeta(10, 2, 3), 1))

  expect_error(capture.output(print(dp)), NA)
  expect_error(capture.output(print(dp, param_summary = TRUE)), NA)

  dpfit <- Fit(dp, 2, progressBar = FALSE)
  expect_error(capture.output(print(dpfit)), NA)
  expect_error(capture.output(print(dpfit, param_summary = TRUE)), NA)
})

test_that("Weibull Printing", {

  dp <- DirichletProcessWeibull(rweibull(10, 2, 3), c(10, 2, 4))

  expect_error(capture.output(print(dp)), NA)
  expect_error(capture.output(print(dp, param_summary = TRUE)), NA)

  dpfit <- Fit(dp, 2, progressBar = FALSE)
  expect_error(capture.output(print(dpfit)), NA)
  expect_error(capture.output(print(dpfit, param_summary = TRUE)), NA)
})

test_that("MvNormal Printing", {
  testData <- matrix(c(rnorm(10), rnorm(10)), ncol = 2)
  dp <- DirichletProcessMvnormal(testData)

  expect_error(capture.output(print(dp)), NA)
  expect_error(capture.output(print(dp, param_summary = TRUE)), NA)

  dpfit <- Fit(dp, 2, progressBar = FALSE)
  expect_error(capture.output(print(dpfit)), NA)
  expect_error(capture.output(print(dpfit, param_summary = TRUE)), NA)
})







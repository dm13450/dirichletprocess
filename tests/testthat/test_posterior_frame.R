context("Posterior Frame")

test_that("Basic Test", {


  testData <- rnorm(100)

  testDP <- DirichletProcessGaussian(testData)
  testDP <- Fit(testDP, 1, progressBar = FALSE)

  testFrame <- PosteriorFrame(testDP, seq(-2, 2, length.out = 10))

  expect_is(testFrame, "data.frame")
  expect_equal(nrow(testFrame), 10)
  expect_equal(ncol(testFrame), 4)
})

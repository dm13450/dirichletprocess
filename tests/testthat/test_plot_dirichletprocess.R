context("Specific Dirichlet process plotting tests")

test_that("Univariate Plotting", {

  dp <- DirichletProcessGaussian(rnorm(10))
  dp <- Fit(dp, 10)

  testPlot <- plot_dirichletprocess_univariate(dp)

  expect_is(testPlot, c("gg", "ggplot"))

})

test_that("Multivariate Plotting", {

  testData <- matrix(c(rnorm(10), rnorm(10, 5)), ncol=2)

  dp <- DirichletProcessMvnormal(testData)
  dp <- Fit(dp, 10)

  testPlot <- plot_dirichletprocess_multivariate(dp)

  expect_is(testPlot, c("gg", "ggplot"))

})

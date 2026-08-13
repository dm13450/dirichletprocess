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

  testPlot <- plot_dirichletprocess_multivariate(dp)
  built <- ggplot2::ggplot_build(testPlot)

  expect_is(testPlot, c("gg", "ggplot"))
  expect_length(built$data, 1)
  expect_equal(nrow(built$data[[1]]), nrow(testData))
  expect_true(all(!is.na(built$data[[1]]$colour)))

})

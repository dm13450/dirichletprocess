context("Gaussian Fixed Variance Mixing Distributions")

test_sigma <- 2
test_mdobj <- GaussianFixedVarianceMixtureCreate(c(0,1), test_sigma)

test_that("Mixture Create", {
  expect_is(test_mdobj, c("list", "normalFixedVariance", "conjugate"))
})

test_that("Likelihood", {
  res <- Likelihood(test_mdobj, 0, list(mu=1))
  expect_equal(res, dnorm(0, 1, test_sigma))
})

test_that("Posterior", {
  res <- PosteriorDraw(test_mdobj, rnorm(10), 10)
  expect_length(res[[1]], 10)
})

test_that("Posterior Parameters", {
  res <- PosteriorParameters(test_mdobj, rnorm(10))
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 2)
})

test_that("Prior", {
  res <- PriorDraw(test_mdobj, 1)
  expect_length(res, 1)
})

test_that("Predictive", {
  res <- Predictive(test_mdobj, 1:10)
  expect_length(res, 10)
})

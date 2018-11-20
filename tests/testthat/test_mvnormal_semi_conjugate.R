context("Multivariate Normal Semi Conjugate Tests")

pp <- list(nu0 = 2,
           phi0 = diag(2),
           mu0 = matrix(c(0, 0),ncol=2),
           sigma0 = diag(2))

test_that("Mixture Object Create", {

  mdobj <- Mvnormal2Create(pp)

  expect_s3_class(mdobj, c("list", "MixingDistribution", "mvnormal", "nonconjugate"))
})

test_that("Multivariate Normal Likelihood", {

  mdobj <- Mvnormal2Create(pp)
  test_theta <- list(mu=array(c(0,0), c(1,2,1)), sig=array(diag(2), c(2,2,1)))
  lik_test <- Likelihood(mdobj, c(0,0), test_theta)

  expect_equal(lik_test, 1/sqrt(4*pi^2))

  test_theta_multi <- list(mu=array(c(0,0), c(1,2,2)), sig=array(diag(2), c(2,2,2)))
  lik_test_multi <- Likelihood(mdobj, c(0,0), test_theta_multi)

  expect_equal(lik_test_multi, rep.int(1/sqrt(4*pi^2), 2))

})

test_that("Multivariate Normal Prior Draw", {

  mdobj <- Mvnormal2Create(pp)

  PriorDraw_test_single <- PriorDraw(mdobj, 1)

  expect_is(PriorDraw_test_single, "list")

  PriorDraw_test_multiple <- PriorDraw(mdobj, 10)

  expect_is(PriorDraw_test_multiple, "list")
  expect_equal(dim(PriorDraw_test_multiple$mu), c(1,2,10))
  expect_equal(dim(PriorDraw_test_multiple$sig), c(2,2,10))

})

test_that("Multivariate Normal Posterior Draw", {

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  mdobj <- Mvnormal2Create(pp)

  post_draws_single <- PosteriorDraw(mdobj, test_data, 1)

  expect_is(post_draws_single, "list")
  expect_equal(length(post_draws_single), 2)
  expect_equal(dim(post_draws_single$mu), c(1,2,1))
  expect_equal(dim(post_draws_single$sig), c(2,2,1))

  post_draws_multi <- PosteriorDraw(mdobj, test_data[1,], 10)

  expect_equal(length(post_draws_multi), 2)
  expect_equal(dim(post_draws_multi$mu), c(1,2,10))
  expect_equal(dim(post_draws_multi$sig), c(2,2,10))
  expect_is(post_draws_multi, "list")
})

test_that("DP Object", {

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  dp <- DirichletProcessMvnormal2(test_data)

  expect_is(dp, c("list", "dirichletprocess", "mvnormal2", "nonconjugate"))

})

test_that("DP Object Fit", {

  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  dp <- DirichletProcessMvnormal2(test_data)
  dp <- Fit(dp, 2)

  expect_is(dp, c("list", "dirichletprocess", "mvnormal2", "nonconjugate"))
  expect_length(dp$likelihoodChain, 2)
  expect_length(dp$alphaChain, 2)

})



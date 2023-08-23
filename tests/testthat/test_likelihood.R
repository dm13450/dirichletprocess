context("Dirichlet Process Likelihood")

num_test_points = 10
data_test = rnorm(num_test_points)
priorParameters_test = matrix(c(1,1,1,1), ncol=4)

normal_object_test = MixingDistribution("normal", priorParameters_test, "conjugate")
dpobj = DirichletProcessCreate(data_test, normal_object_test)
dpobj = Initialise(dpobj)

test_that("Likelihood Function Test", {

  Likelihood_function_test = LikelihoodFunction(dpobj)

  expect_is(Likelihood_function_test, "function")

})

test_that("Likelihood Values Test", {
  set.seed(1406)
  n  <- 100
  y <- rt(n, 3) + 2

  g0Priors <- c(0, 1, 1, 1)
  alphaPriors <- c(2, 4)

  mdobj <- GaussianMixtureCreate(g0Priors)
  dpobj <- DirichletProcessCreate(y, mdobj, alphaPriors)
  dpobj <- Initialise(dpobj)
  dpobj <- Fit(dpobj, 1000)
  likelihoodVals <- LikelihoodDP(dpobj)

  expect_length(likelihoodVals, n)

})

test_that("Multivariate Likelihood Values Test", {
  test_data <- mvtnorm::rmvnorm(10, c(0,0), diag(2))
  priorParameters <- list(mu0=c(0,0), Lambda=diag(2), kappa0=1, nu=2)
  mdobj <- MvnormalCreate(priorParameters)

  dpobj <- DirichletProcessCreate(test_data, mdobj)
  dpobj <- Initialise(dpobj)
  dpobj <- Fit(dpobj, 10, FALSE, FALSE)
  likelihoodVals <- LikelihoodDP(dpobj)

  expect_length(likelihoodVals, 10)

})







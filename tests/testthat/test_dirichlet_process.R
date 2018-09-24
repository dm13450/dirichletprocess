context("Dirichlet Fit Functions")

num_test_points = 10
data_test = rnorm(num_test_points)
dpobj <- DirichletProcessGaussian(data_test)

test_that("Dirichlet Process Create", {
  expect_equal(class(dpobj), c("list", "dirichletprocess", "normal", "conjugate"))
  expect_equal(dpobj$data, matrix(data_test, ncol=1))
  expect_equal(dpobj$n, length(data_test))
})


test_that("Dirichlet Conjugate Process Fit", {

  dpobj = Fit(dpobj, 10, FALSE, FALSE)

  expect_equal(dpobj$data, matrix(data_test, ncol=1))
  expect_equal(dpobj$n, length(data_test))

  expect_equal(length(dpobj$clusterLabels), num_test_points)
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)

  expect_equal(length(dpobj$alphaChain), 10)
  expect_equal(length(dpobj$weightsChain), 10)
  expect_equal(length(dpobj$clusterParametersChain), 10)

})

test_that("Dirichlet Nonconjugate Process Fit", {

  num_test_points = 10
  data_test = rweibull(num_test_points, 1,1)
  priorParameters_test = matrix(c(1,1,1), ncol=3)
  weibull_object_test <- WeibullMixtureCreate(priorParameters_test, 1)

  dpobj <- DirichletProcessCreate(data_test, weibull_object_test)
  dpobj <- Initialise(dpobj, verbose=FALSE)
  dpobj = Fit(dpobj, 10, FALSE, FALSE)

  expect_equal(dpobj$data, matrix(data_test, ncol=1))
  expect_equal(dpobj$n, length(data_test))

  expect_equal(length(dpobj$clusterLabels), num_test_points)
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)

  expect_equal(length(dpobj$alphaChain), 10)
  expect_equal(length(dpobj$weightsChain), 10)
  expect_equal(length(dpobj$clusterParametersChain), 10)

})

test_that("Dirichlet Nonconjugate Procees Fit Prior Parameter Update", {

  num_test_points = 10
  data_test = rweibull(num_test_points, 1,1)
  priorParameters_test = matrix(c(1,1,1), ncol=3)

  dpobj <- DirichletProcessWeibull(data_test, priorParameters_test)
  dpobj = Fit(dpobj, 10, TRUE, FALSE)

  expect_equal(dpobj$data, matrix(data_test, ncol=1))
  expect_equal(dpobj$n, length(data_test))

  pd <- PriorDraw(dpobj$mixingDistribution, 1000)
  which(is.na(Likelihood(dpobj$mixingDistribution, 0.1, pd)))


  expect_equal(length(dpobj$clusterLabels), num_test_points)
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)

  expect_length((dpobj$alphaChain), 10)
  expect_length((dpobj$weightsChain), 10)
  expect_length((dpobj$clusterParametersChain), 10)
  expect_length((dpobj$priorParametersChain), 10)

})

test_that("Dirichlet Process Vary mhDraws", {

  num_test_points = 10
  data_test = rweibull(num_test_points, 1,1)
  priorParameters_test = matrix(c(1,1,1), ncol=3)

  dpobj <- DirichletProcessWeibull(data_test, priorParameters_test, verbose = FALSE, mhDraws = 300)

  expect_equal(dpobj$mhDraws, 300)


})





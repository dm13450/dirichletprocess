context("Dirichlet Process Posterior")

num_test_points <- 10
data_test <- rnorm(num_test_points)
priorParameters_test <- matrix(c(1,1,1,1), ncol=4)

normal_object_test <- MixingDistribution("normal", priorParameters_test, "conjugate")
dpobj = DirichletProcessCreate(data_test, normal_object_test)
dpobj = Initialise(dpobj)
dpobj = Fit(dpobj, 10, FALSE, FALSE)

test_that("Posterior Clusters Default", {

  postClusters <- PosteriorClusters(dpobj)

  expect_is(postClusters, "list")
  expect_equal(length(postClusters$params), length(dpobj$clusterParameters))

})

test_that("Posterior Clusters Ind", {

  postClusters <- PosteriorClusters(dpobj, 7)

  expect_is(postClusters, "list")
  expect_equal(length(postClusters$params), length(dpobj$clusterParameters))
})


test_that("Posterior Function", {

  post_function <- PosteriorFunction(dpobj)

  expect_is(post_function, "function")
  expect_is(post_function(0), "numeric")

})

test_that("Posterior Clusters: MvNormal", {

  y <- mvtnorm::rmvnorm(10, c(0,0), diag(2))

  dp <- DirichletProcessMvnormal(y)
  dp <- Fit(dp, 1, FALSE, FALSE)

  postClusters <- PosteriorClusters(dp)

  expect_equal(length(postClusters), 2)

})

test_that("Posterior Function: MvNormal", {

  y <- mvtnorm::rmvnorm(10, c(0,0), diag(2))

  dp <- DirichletProcessMvnormal(y)
  dp <- Fit(dp, 1, FALSE, FALSE)

  postFunc <- PosteriorFunction(dp)
  postFuncEval <- postFunc(matrix(c(0,0), ncol=2))

  expect_is(postFunc, "function")
  expect_is(postFuncEval, "numeric")
  expect_equal(length(postFuncEval), 1)

})




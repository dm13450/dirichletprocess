context("Prior Clusters")

test_that("Prior Clusters", {

  dp <- DirichletProcessGaussian(rnorm(100))
  priorclusters <- PriorClusters(dp)

  expect_length(priorclusters, 2)
  expect_is(priorclusters, "list")

})

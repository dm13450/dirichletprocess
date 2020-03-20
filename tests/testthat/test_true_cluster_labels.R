context("True cluster labels")

require(mvtnorm)
testData <- list(rmvnorm(200, c(3,3), diag(2)), rmvnorm(200, c(0,0), diag(2)))
hdp <- DirichletProcessHierarchicalMvnormal2(testData)

test_that("Numeric vector", {
  
  vec <- c(1,2,3,4,5,6)
  
  vec <- true_cluster_labels(vec,hdp)
  expect_equal(vec, c(1,2,3))
})


test_that("Empty vector", {
  
  vec <- c()
  
  vec <- true_cluster_labels(vec,hdp)
  expect_length(vec, 0)
})




test_that("1D data", {
  
  testData <- replicate(3, rbeta(100, 3, 6), simplify = FALSE)
  hdp <- DirichletProcessHierarchicalBeta(testData, 1)
  vec <- c(1,2,3)
  
  vec <- true_cluster_labels(vec,hdp)
  expect_equal(vec, c(1,2,3))
})

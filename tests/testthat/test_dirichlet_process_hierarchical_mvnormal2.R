context("Hierarchical Mv Normal")

test_that("",{
  require(mvtnorm)
  testData <- replicate(3, rmvnorm(500, c(3,3), diag(2)), simplify = FALSE)
  g0Priors <- list(nu0 = 2,
                   phi0 = diag(2),
                   mu0 = matrix(c(0, 0),ncol=2),
                   sigma0 = diag(2))
  
  dp <- DirichletProcessHierarchicalMvnormal2(testData, g0Priors, gammaPriors = c(2, 0.01))
  expect_s3_class(dp, c("list", "dirichletprocess", "hierarchical"))
  
})

test_that("Hierarchical Mvnormal2 default numInitialClusters keeps one local cluster per group", {
  set.seed(11)
  testData <- replicate(2, mvtnorm::rmvnorm(8, c(1, -1), diag(2)), simplify = FALSE)

  dp <- DirichletProcessHierarchicalMvnormal2(testData, numInitialClusters = 1)

  expect_equal(vapply(dp$indDP, function(x) x$numberClusters, numeric(1)), c(1, 1))
  expect_equal(lapply(dp$indDP, function(x) as.numeric(x$pointsPerCluster)),
               list(8, 8))
})

test_that("Hierarchical Mvnormal2 numInitialClusters is passed through to local initialisation", {
  set.seed(12)
  testData <- replicate(2, mvtnorm::rmvnorm(8, c(0, 0), diag(2)), simplify = FALSE)

  dp <- DirichletProcessHierarchicalMvnormal2(testData, numInitialClusters = 3)

  expect_equal(vapply(dp$indDP, function(x) x$numberClusters, numeric(1)), c(3, 3))
  expect_equal(lapply(dp$indDP, function(x) as.numeric(x$pointsPerCluster)),
               list(c(3, 3, 2), c(3, 3, 2)))
})

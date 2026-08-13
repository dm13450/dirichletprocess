context("Dirichlet Process Beta 2")

test_that("Create",{

  testData <- rbeta(10, 2, 5)

  dp <- DirichletProcessBeta2(testData, 1)

  expect_equal(class(dp), c("dirichletprocess", "list"))
  expect_false(inherits(dp, "beta2"))
  expect_false(inherits(dp, "nonconjugate"))

})

test_that("Fit", {

  testData <- rbeta(10, 2, 5)

  dp <- DirichletProcessBeta2(testData, 1)

  dp <- Fit(dp, 10, FALSE, FALSE)

  expect_equal(class(dp), c("dirichletprocess", "list"))

  expect_is(dp$clusterParameters, "list")
  expect_length(dp$clusterParameters, 2)

  expect_length(dp$clusterParametersChain, 10)
  expect_length(dp$alphaChain, 10)
  expect_length(dp$weightsChain, 10)

})

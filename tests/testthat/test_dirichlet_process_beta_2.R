context("Dirichlet Process Beta 2")

test_that("Create",{

  testData <- rbeta(10, 2, 5)

  dp <- DirichletProcessBeta2(testData, 1)

  expect_is(dp, c("list", "dirichletprocess", "beta2", "nonconjugate"))

})

test_that("Fit", {

  testData <- rbeta(10, 2, 5)

  dp <- DirichletProcessBeta2(testData, 1)

  dp <- Fit(dp, 10, FALSE, FALSE)

  expect_is(dp, c("list", "dirichletprocess", "beta2", "nonconjugate"))

  expect_is(dp$clusterParameters, "list")
  expect_length(dp$clusterParameters, 2)

  expect_length(dp$clusterParametersChain, 10)
  expect_length(dp$alphaChain, 10)
  expect_length(dp$weightsChain, 10)

})

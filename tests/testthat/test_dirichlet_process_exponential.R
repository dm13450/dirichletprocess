context("Dirichlet Process Exponential")

testData <- rexp(10)

test_that("Creation", {

  dp <- DirichletProcessExponential(testData)

  expect_is(dp, c("list", "dirichletprocess", "exponenital", "cojugate"))
})

test_that("Fit", {

  dp <- DirichletProcessExponential(testData)
  dp <- Fit(dp, 10, FALSE, FALSE)

  expect_is(dp, c("list", "dirichletprocess", "exponenital", "cojugate"))

  expect_is(dp$clusterParameters, "list")
  expect_length(dp$clusterParameters, 1)

  expect_length(dp$clusterParametersChain, 10)
  expect_length(dp$alphaChain, 10)
  expect_length(dp$weightsChain, 10)

})

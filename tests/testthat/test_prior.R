context("Prior Function")

test_that("Prior Function", {

  dp <- DirichletProcessGaussian(rnorm(100))

  priorF <- PriorFunction(dp)

  expect_is(priorF, "function")
  expect_is(priorF(0), "numeric")
})

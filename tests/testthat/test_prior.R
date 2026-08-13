context("Prior Function")

# Access function from namespace if not available in global environment
if (!exists("PriorFunction")) {
  PriorFunction <- get("PriorFunction", getNamespace("dirichletprocess"))
}

test_that("Prior Function", {

  dp <- DirichletProcessGaussian(rnorm(100))

  priorF <- PriorFunction(dp)

  expect_is(priorF, "function")
  expect_is(priorF(0), "numeric")
})

context("Exponential Mixing Distribution Tests")


test_mdobj <- ExponentialMixtureCreate(c(0.1, 0.1))

test_that("Creation", {
  expect_is(test_mdobj, c("list", "exponential", "conjugate"))
})

test_that("Likelihood: 1 Parameter", {
  testTheta <- list(array(1, dim=c(1,1,1)))
  testEval <- Likelihood(test_mdobj, 0, testTheta)

  expect_is(testEval, "numeric")
  expect_equal(testEval, dexp(0, 1))
})

test_that("Likelihood: 2 Parameter", {
  testTheta <- list(array(c(1,2), dim=c(1,1,2)))
  testEval <- Likelihood(test_mdobj, 0, testTheta)

  expect_is(testEval, "numeric")
  expect_equal(testEval, dexp(0, c(1, 2)))
  expect_length(testEval, 2)
})

test_that("Prior Draw: 1 Draw", {

  testPriorDraws <- PriorDraw(test_mdobj, 1)

  expect_is(testPriorDraws, "list")
  expect_is(testPriorDraws[[1]], "array")
  expect_equal(dim(testPriorDraws[[1]]), c(1,1,1))
})

test_that("Prior Draw: 10 Draw", {

  testPriorDraws <- PriorDraw(test_mdobj, 10)

  expect_is(testPriorDraws, "list")
  expect_is(testPriorDraws[[1]], "array")
  expect_equal(dim(testPriorDraws[[1]]), c(1,1,10))
})


test_that("Posterior Draw: 1 Draw", {

  testPosteriorDraws <- PosteriorDraw(test_mdobj, rexp(10, 2), 1)

  expect_is(testPosteriorDraws, "list")
  expect_is(testPosteriorDraws[[1]], "array")
  expect_equal(dim(testPosteriorDraws[[1]]), c(1,1,1))
})

test_that("Posterior Draw: 10 Draw", {

  testPosteriorDraws <- PosteriorDraw(test_mdobj, rexp(10, 2), 10)

  expect_is(testPosteriorDraws, "list")
  expect_is(testPosteriorDraws[[1]], "array")
  expect_equal(dim(testPosteriorDraws[[1]]), c(1,1,10))
})


test_that("Predictive", {

  testPred <- Predictive(test_mdobj, rexp(10))

  expect_is(testPred, "numeric")
  expect_length(testPred, 10)
})

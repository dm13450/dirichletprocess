context("Hidden Markov Model")

testData <- c(rnorm(50, 1, sqrt(3)), rnorm(50, 3, sqrt(3)), rnorm(50, 5, sqrt(3)))
normMD <- GaussianMixtureCreate()

HMM_dp_test <- function(dp){

  expectedValues <- c("data", "mdobj", "states", "params", "alpha", "beta")

  expect_is(dp, "list")
  expect_length(dp, length(expectedValues))
  expect_equal(names(dp), expectedValues)
  expect_is(dp$states, "integer")
  expect_length(dp$states, length(dp$data))
  expect_length(dp$params, length(dp$data))
  expect_length(dp$alpha, 1)
  expect_length(dp$beta, 1)

}

test_that("Create",{

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)

  ##Add in class checks

  HMM_dp_test(dp)


})

test_that("Update States Integration", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)

  dp <- UpdateStates(dp)

  HMM_dp_test(dp)

})

test_that("Update Parameters Integration", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)
  dp <- UpdateStates(dp)
  dp <- param_update(dp)

  HMM_dp_test(dp)

})

test_that("Fit", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)
  dp <- fit_hmm(dp, 10)

  expect_is(dp, "list")
  expect_is(dp$states, "integer")
  expect_length(dp$states, length(dp$data))
  expect_length(dp$params, length(dp$data))
  expect_length(dp$alpha, 1)
  expect_length(dp$beta, 1)

})



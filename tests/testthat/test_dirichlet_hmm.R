context("Hidden Markov Model")

testData <- c(rnorm(50, 1, sqrt(3)), rnorm(50, 3, sqrt(3)), rnorm(50, 5, sqrt(3)))
normMD <- GaussianMixtureCreate()

HMM_dp_test <- function(dp){

  expect_is(dp, c("list", "markov", "dirichetprocess", "normal", "conjugate"))
  expect_is(dp$states, "integer")
  expect_length(dp$states, length(dp$data))
  expect_length(dp$params, length(dp$data))
  expect_length(dp$alpha, 1)
  expect_length(dp$beta, 1)

}

test_that("Create",{

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)


  expectedValues <- c("data", "mdobj", "states", "params", "alpha", "beta")
  expect_equal(names(dp), expectedValues)

  HMM_dp_test(dp)


})

test_that("Update States Integration", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)

  dp <- UpdateStates(dp)

  expectedValues <- c("data", "mdobj", "states", "params", "alpha", "beta")
  expect_equal(names(dp), expectedValues)

  HMM_dp_test(dp)

})

test_that("Update Parameters Integration", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)
  dp <- UpdateStates(dp)
  dp <- param_update(dp)

  expectedValues <- c("data", "mdobj", "states", "params", "alpha", "beta")
  expect_equal(names(dp), expectedValues)

})

test_that("Fit Inner", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)
  dp <- fit_hmm(dp, 10)

  HMM_dp_test(dp)


})

test_that("Fit Dispatch", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)
  dp <- Fit(dp, 10)

  HMM_dp_test(dp)

})


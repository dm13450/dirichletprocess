context("Hidden Markov Model")

# Helper function to ensure state parameters have correct structure
ensure_state_params <- function(params) {
  if (!is.list(params)) {
    # If it's a single value, assume it's the mean with sd = 1
    return(list(mean = params, sd = 1))
  }

  # Ensure both mean and sd exist
  if (!("mean" %in% names(params))) {
    params$mean <- 0  # default mean
  }
  if (!("sd" %in% names(params))) {
    params$sd <- 1   # default sd
  }

  return(params)
}

testData <- c(rnorm(50, 1, sqrt(3)), rnorm(50, 3, sqrt(3)), rnorm(50, 5, sqrt(3)))
normMD <- GaussianMixtureCreate()

expNames <- c("data", "n", "mixingDistribution", "states", "uniqueParams", "params", "alpha", "beta")

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

  expectedValues <- expNames
  expect_equal(names(dp), expectedValues)

  HMM_dp_test(dp)
})

test_that("Update States Integration", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)

  # Access function from namespace if not available in global environment
  if (!exists("UpdateStates")) {
    UpdateStates <- get("UpdateStates", getNamespace("dirichletprocess"))
  }
  dp <- UpdateStates(dp)

  expectedValues <- expNames
  expect_equal(names(dp), expectedValues)

  HMM_dp_test(dp)
})

test_that("Update Parameters Integration", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)
  # Access function from namespace if not available in global environment
  if (!exists("UpdateStates")) {
    UpdateStates <- get("UpdateStates", getNamespace("dirichletprocess"))
  }
  dp <- UpdateStates(dp)
  # Access function from namespace if not available in global environment
  if (!exists("param_update")) {
    param_update <- get("param_update", getNamespace("dirichletprocess"))
  }
  dp <- param_update(dp)

  expectedValues <- expNames
  expect_equal(names(dp), expectedValues)

})

test_that("Fit Inner", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)
  # Access function from namespace if not available in global environment
  if (!exists("fit_hmm")) {
    fit_hmm <- get("fit_hmm", getNamespace("dirichletprocess"))
  }
  dp <- fit_hmm(dp, 10)

  HMM_dp_test(dp)


})

test_that("Fit Dispatch", {

  dp <- DirichletHMMCreate(testData, normMD, 2, 3)
  dp <- Fit(dp, 10)

  HMM_dp_test(dp)

})
